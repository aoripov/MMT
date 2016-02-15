package info.kwarc.mmt.api.archives

import java.util.concurrent.ConcurrentLinkedDeque

import info.kwarc.mmt.api.Level.Level
import info.kwarc.mmt.api._
import frontend._
import utils._
import web.{Body, Server, ServerExtension}

import scala.collection.JavaConverters._
import scala.collection.mutable

/** */
class QueuedTask(val target: TraversingBuildTarget, val task: BuildTask) {
  /** task should be queued at end */
  var lowPriority: Boolean = true

  def highPriority = !lowPriority

  /** task was not requested directly but added as dependency of some other task */
  var dependencyClosure: Boolean = false

  /** dependencies that will be used but are not available */
  var missingDeps: Iterable[Dependency] = Nil
  /** resources that will be provided once successfully built */
  var willProvide: List[ResourceDependency] = Nil

  def toJson: JSONString = {
    val str = task.inPath.toString
    JSONString((if (str.isEmpty) task.archive.id else str) + " (" + target.key + ")")
  }

  def rebuildNeeded(level: Level): Boolean = {
    target.rebuildNeeded(missingDeps.toSet, task, level)
  }
}

/** */
sealed abstract class BuildResult {
  /**
    * resources that were used during building
    */
  def used: List[ResourceDependency]

  /** resources that have been built successfully */
  def provided: List[ResourceDependency]

  def toJson: JSON

  def toJsonPart: List[(String, JSON)] =
    List(("needed", JSONArray()),
      ("used", JSONArray(used.map(_.toJson): _*)),
      ("provided", JSONArray(provided.map(_.toJson): _*)))
}

object BuildResult {
  def empty: BuildResult = BuildSuccess(Nil, Nil)
}

/** successful build */
case class BuildSuccess(used: List[ResourceDependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("success", JSONBoolean(true)) :: toJsonPart: _*)
}

/** unrecoverable failure */
case class BuildFailure(used: List[ResourceDependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("success", JSONBoolean(false)) :: toJsonPart: _*)
}

/** recoverable failure: build should be retried after building a missing dependency */
case class MissingDependency(needed: List[ResourceDependency], provided: List[ResourceDependency]) extends BuildResult {
  def used = Nil

  def toJson: JSON = JSONObject(("success", JSONBoolean(false)) ::
    ("needed", JSONArray(needed.map(_.toJson): _*)) :: toJsonPart.tail: _*)
}


/** dependency of a [[QueuedTask]] */
sealed abstract class Dependency {
  def toJson: JSONString
}

sealed abstract class BuildDependency extends Dependency {
  def key: String

  def archive: Archive

  def inPath: FilePath

  def getTarget(controller: Controller): TraversingBuildTarget =
    controller.extman.getOrAddExtension(classOf[TraversingBuildTarget], key).getOrElse {
      throw RegistrationError("build target not found: " + key)
    }

  def getErrorFile(controller: Controller): File
}

/** dependency on another [[BuildTask]]
  *
  * @param inPath path to file (without inDim)
  */
case class FileBuildDependency(key: String, archive: Archive, inPath: FilePath) extends BuildDependency {
  def toJson: JSONString = JSONString(inPath.toString + " (" + key + ")")

  def getErrorFile(controller: Controller): File = (archive / errors / key / inPath).addExtension("err")
}

/** like [[FileBuildDependency]] but for a directory
  *
  * @param inPath path to file (without inDim)
  */
case class DirBuildDependency(key: String, archive: Archive, inPath: FilePath, children: List[BuildTask])
  extends BuildDependency {
  def toJson: JSONString = JSONString(archive.id + "/" + inPath.toString +
    " (" + key + ") " + children.map(bt => bt.inPath).mkString("[", ", ", "]"))

  def getErrorFile(controller: Controller): File = getTarget(controller).getFolderErrorFile(archive, inPath)
}

sealed abstract class ResourceDependency extends Dependency

/** a dependency on a physical resource
  */
case class PhysicalDependency(file: File) extends ResourceDependency {
  def toJson: JSONString = JSONString(file.toString)
}

/** a dependency on an MMT module that must be provided by building some other [[BuildTask]]
  *
  * providing the dependency typically requires some catalog to determine the appropriate [[BuildTask]]
  */
case class LogicalDependency(mpath: MPath) extends ResourceDependency {
  def toJson: JSONString = JSONString(MPath.toString)
}

/** a dependency on an externally provided source file that cannot be built by MMT
  *
  * this cannot be discharged by MMT, but changes to the file can trigger rebuilding
  */
case class ForeignDependency(file: File) extends Dependency {
  def toJson: JSONString = JSONString(file.toString)
}


/** handles build tasks generated by a [[TraversingBuildTarget]] */
abstract class BuildManager extends Extension {
  def addTasks(up: Update, qts: Iterable[QueuedTask])

  def waitToEnd

  protected def isUpToDate(update: Update, bd: BuildDependency): Boolean = bd match {
    case fbd: FileBuildDependency =>
      val target = fbd.getTarget(controller)
      val inFile = fbd.archive / target.inDim / fbd.inPath
      val errFile = fbd.getErrorFile(controller)
      !target.modified(inFile, errFile)
    case dbd: DirBuildDependency => true // ignore for now
  }
}

/** builds tasks immediately (no queueing, no dependency management, no parallel processing) */
class TrivialBuildManager extends BuildManager {
  def addTasks(up: Update, qts: Iterable[QueuedTask]) =
    qts.foreach { qt =>
          qt.target.checkOrRunBuildTask(qt.missingDeps.toSet, qt.task, up)
        }

  // no need to wait
  def waitToEnd {}
}

/** queues build tasks for multi-threaded execution, includes dependency management */
class BuildQueue extends BuildManager {
  private val queued = new ConcurrentLinkedDeque[QueuedTask]
  private var blocked: List[QueuedTask] = Nil

  /** all tasks currently in the queue */
  val alreadyQueued = new mutable.HashMap[Dependency, QueuedTask]
  /** all tasks that were built (successfully or permanently-failing) since the last time the queue was empty */
  val alreadyBuilt = new mutable.HashMap[Dependency, BuildResult]
  var finishedBuilt = List[(Dependency, BuildResult)]()

  private var continue: Boolean = true
  private var stopOnEmpty: Boolean = false

  val sleepTime: Int = 2000

 private def addTask(up: Update, qt: QueuedTask) {
    val qtDep = qt.task.asDependency
    if (alreadyBuilt isDefinedAt qtDep) {
      if (qt.dependencyClosure) {
        // dependency of previous job: skip
        return
      } else {
        // new job: build anew
        alreadyBuilt -= qtDep
      }
    }
    if (alreadyQueued isDefinedAt qtDep) {
      if (qt.lowPriority) {
        // low priority: no need to add, skip
        return
      } else {
        // high priority: queue again
        queued.remove(alreadyQueued(qtDep))
        alreadyQueued -= qtDep
      }
    }
    // add to front/end of queue depending on priority
    if (qt.lowPriority) {
      queued.addLast(qt)
    } else {
      queued.addFirst(qt)
    }
    alreadyQueued(qtDep) = qt
  }

  def addTasks(up: Update, qts: Iterable[QueuedTask]) {
    synchronized {
      qts.foreach(addTask(up, _))
    }
  }

  /** recursively queues all dependencies of the next task; then returns the head of the queue */
  private def getTopTask: (Option[QueuedTask], Iterable[Dependency]) = synchronized {
    val optQt = Option(queued.poll)
    optQt.foreach(qt => alreadyQueued -= qt.task.asDependency)
    val currentMissingDeps = optQt match {
      case None => Nil
      case Some(qt) => qt.missingDeps.flatMap {
        case bd: FileBuildDependency =>
          List(bd)
        case bd: DirBuildDependency =>
          // skip for now
          // TODO
          Nil
        case r: ResourceDependency =>
          List(r)
        case fd: ForeignDependency =>
          // should not happen, cannot be handled at this point
          Nil
      }
    }
    (optQt, currentMissingDeps)
  }

  private def getNextTask: Option[QueuedTask] = {
    val (optQt, currentMissingDeps) = getTopTask
    val (bDeps, fDeps) = currentMissingDeps.partition {
      case bd: FileBuildDependency => true
      case _ => false
    }
    val bds = bDeps.collect { case bd: BuildDependency => bd }
    if (currentMissingDeps.nonEmpty) {
      val qt = optQt.get // is non-empty if deps are missing
      if (fDeps.nonEmpty) {
        qt.missingDeps = fDeps
        blocked = blocked ::: List(qt)
        getNextTask
      } else {
        val updatePolicy = Update(Level.Ignore) // fix up-to-date policy for now
        val (_, ts) = bds.partition(isUpToDate(updatePolicy, _))
        if (ts.isEmpty) optQt
        else {
          queued.addFirst(qt)
          ts.foreach(t => buildDependency(updatePolicy, qt.task.asDependency, t))
          getNextTask
        }
      }
    } else optQt
  }

  private def findResource(r: ResourceDependency): Option[FileBuildDependency] = r match {
    case PhysicalDependency(f) =>
      val (root, out) = controller.backend.resolveAnyPhysical(f).getOrElse(return None)
      controller.addArchive(root)
      val a = controller.backend.getArchive(root).getOrElse(return None)
      out match {
        case FilePath("export" :: key :: _) =>
          // a resource generated by an [[Exporter]]
          val exp = controller.extman.get(classOf[Exporter], key).getOrElse(return None)
          val in = exp.producesFrom(out).getOrElse(return None)
          val bd = FileBuildDependency(key, a, in)
          Some(bd)
        case fp if fp.startsWith(a.resolveDimension(source)) =>
          val imp = controller.extman.get(classOf[Importer], ???).getOrElse(return None) //TODO what importer to use?
        val in = imp.producesFrom(out).getOrElse(return None)
          val bd = FileBuildDependency(imp.key, a, in)
          Some(bd)
        case _ =>
          // TODO lookup in some other way
          None
      }
    case LogicalDependency(mp) =>
      // TODO lookup in some catalog, details TBD
      None
  }

  private def buildDependency(up: Update, top: Dependency, bd: BuildDependency) = bd match {
    case fbd: FileBuildDependency =>
    val qts = fbd.getTarget(controller).makeBuildTasks(fbd.archive, fbd.inPath, None).map {
      qt =>
        qt.lowPriority = false
        qt.dependencyClosure = true
        qt.missingDeps = qt.missingDeps.filter(d => d != top)
        qt
    }
    addTasks(up, qts)
    case dbd: DirBuildDependency => // ignore for now
  }

  /** unblock previously blocked tasks whose dependencies have now been provided */
  private def unblockTasks(res: BuildResult) {
    blocked.foreach { bt =>
      bt.missingDeps = bt.missingDeps.toList diff res.provided
    }
    val (unblocked, stillBlocked) = blocked.partition(_.missingDeps.isEmpty)
    blocked = stillBlocked
    unblocked.reverseMap(queued.add)
  }

  override def start(args: List[String]) {
    controller.extman.addExtension(serve)
    buildThread.start
  }

  override def destroy {
    synchronized {
      continue = false
    }
  }

  def destroyWhenQueueEmpty {
    synchronized {
      stopOnEmpty = true
    }
  }

  val buildThread = new Thread {
    override def run {
      while (continue) {
        getNextTask match {
          case Some(qt) =>
            // TODO run this in a Future
            val res = qt.target.runBuildTask(qt.task)
            val qtDep = qt.task.asDependency
            finishedBuilt ::=(qtDep, res)
            res match {
              case _: BuildSuccess | _: BuildFailure =>
                // remember finished build
                alreadyBuilt(qtDep) = res
              case MissingDependency(missing, provided) =>
                // register missing dependencies and requeue
                qt.missingDeps = missing
                blocked = blocked ::: List(qt)
            }
            unblockTasks(res)
          case None =>
            alreadyBuilt.clear
            if (stopOnEmpty)
              continue = false
            else
              Thread.sleep(sleepTime)
        }
      }
    }
  }

  def waitToEnd: Unit = {
    destroyWhenQueueEmpty
    buildThread.join()
  }

  def getQueueInfo: JSON = synchronized {
    val iter = queued.iterator.asScala
    val q = iter.toList.map(_.toJson)
    val bs = blocked.map(_.toJson)
    val fs = finishedBuilt.map {
      case (d, r) =>
        JSONObject(("dependency", d.toJson), ("result", r.toJson))
    }
    JSONObject(("queue", JSONArray(q: _*)),
      ("blocked", JSONArray(bs: _*)),
      ("finished", JSONArray(fs: _*)))
  }

  /** serves lists of [[Error]]s */
  private val serve = new ServerExtension("queue") {
    def apply(path: List[String], query: String, body: Body) = path match {
      case _ =>
        Server.JsonResponse(getQueueInfo)
    }
  }
}
