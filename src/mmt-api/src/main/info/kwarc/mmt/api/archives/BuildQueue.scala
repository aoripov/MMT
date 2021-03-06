package info.kwarc.mmt.api.archives

import java.util.concurrent.ConcurrentLinkedDeque

import info.kwarc.mmt.api._
import frontend._
import utils._
import web._

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue
import scala.collection.mutable

/** */
class QueuedTask(val target: TraversingBuildTarget, val task: BuildTask) {
  /** task should be queued at end */
  var lowPriority: Boolean = true

  def highPriority = !lowPriority

  /** task was not requested directly but added as dependency of some other task */
  var dependencyClosure: Boolean = false

  /** task was eventually run despite being blocked due to missing dependencies */
  var forceRun: List[Dependency] = Nil

  private val estRes = target.estimateResult(task)

  /** dependencies that are needed for an up-to-date check */
  val neededDeps: List[Dependency] = estRes.used

  /** dependencies that will be used but are not available */
  var missingDeps: List[Dependency] = estRes.used
  /** resources that will be provided once successfully built */
  var willProvide: List[ResourceDependency] = estRes.provided
  /** update policy */
  var updatePolicy = Update(Level.Force)

  def toJString: String = {
    val str = task.inPath.toString
    (if (str.isEmpty) task.archive.id else str) + " (" + target.key + ")" +
      missingDeps.map(_.toJString).mkString(" [", ", ", "]")
  }

  def toJson: JSONString = JSONString(toJString)

  def merge(qt: QueuedTask): Unit = {
    updatePolicy = updatePolicy.merge(qt.updatePolicy)
    lowPriority = lowPriority && qt.lowPriority
    dependencyClosure = dependencyClosure && qt.dependencyClosure
    // not sure if missingDeps and willProvide should be merged
  }
}

/** */
sealed abstract class BuildResult {
  /**
    * resources that were used during building
    */
  def used: List[Dependency]

  /** resources that have been built successfully */
  def provided: List[ResourceDependency]

  def toJson: JSON

  def toJsonPart: List[(String, JSON)] =
    List(("needed", JSONArray()),
      ("used", JSONArray(used.map(_.toJson): _*)),
      ("provided", JSONArray(provided.map(_.toJson): _*)))
}

object BuildResult {
  def empty: BuildSuccess = BuildSuccess(Nil, Nil)
  /** convenience method to create the result of successfully importing a (typically) externally checked document */
  def fromImportedDocument(doc: documents.Document) = {
    val provs = doc.getDeclarations collect {
      case r: documents.MRef => LogicalDependency(r.target)
    }
    BuildSuccess(Nil, provs)
  }
}

case class BuildEmpty(str: String) extends BuildResult {
  def used: List[Dependency] = Nil

  def provided: List[ResourceDependency] = Nil

  def toJson: JSON = JSONObject(("result", JSONString(str)) :: toJsonPart: _*)
}

/** successful build */
case class BuildSuccess(used: List[Dependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("result", JSONString("success")) :: toJsonPart: _*)
}

/** unrecoverable failure */
case class BuildFailure(used: List[Dependency], provided: List[ResourceDependency]) extends BuildResult {
  def toJson: JSON = JSONObject(("result", JSONString("failure")) :: toJsonPart: _*)
}

/** recoverable failure: build should be retried after building a missing dependency */
case class MissingDependency(needed: List[Dependency], provided: List[ResourceDependency]) extends BuildResult {
  def used = Nil

  def toJson: JSON = JSONObject(("result", JSONString("failed")) ::
    ("needed", JSONArray(needed.map(_.toJson): _*)) :: toJsonPart.tail: _*)
}


/** dependency of a [[QueuedTask]] */
sealed abstract class Dependency {
  /** convert to a string for toJson */
  def toJString: String

  def toJson: JSONString = JSONString(toJString)
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
  def toJString: String = inPath.toString + " (" + key + ")"

  def getErrorFile(controller: Controller): File = (archive / errors / key / inPath).addExtension("err")
}

/** like [[FileBuildDependency]] but for a directory
  *
  * @param inPath path to file (without inDim)
  */
case class DirBuildDependency(key: String, archive: Archive, inPath: FilePath, children: List[BuildTask])
  extends BuildDependency {
  def toJString: String = archive.id + "/" + inPath.toString +
    " (" + key + ") " + children.map(bt => bt.inPath).mkString("[", ", ", "]")

  def getErrorFile(controller: Controller): File = getTarget(controller).getFolderErrorFile(archive, inPath)
}

sealed abstract class ResourceDependency extends Dependency

/** a dependency on a physical resource
  */
case class PhysicalDependency(file: File) extends ResourceDependency {
  def toJString: String = file.toString
}

/** a dependency on an MMT module that must be provided by building some other [[BuildTask]]
  *
  * providing the dependency typically requires some catalog to determine the appropriate [[BuildTask]]
  */
case class LogicalDependency(mpath: MPath) extends ResourceDependency {
  def toJString: String = mpath.toString
}

/** handles build tasks generated by a [[TraversingBuildTarget]] */
abstract class BuildManager extends Extension {
  def addTasks(up: Update, qts: Iterable[QueuedTask])

  def waitToEnd: Unit
}

/** builds tasks immediately (no queueing, no dependency management, no parallel processing) */
class TrivialBuildManager extends BuildManager {
  def addTasks(up: Update, qts: Iterable[QueuedTask]) =
    qts.foreach { qt =>
      qt.target.checkOrRunBuildTask(Set(), qt.task, up)
    }

  // no need to wait
  def waitToEnd {}
}

/** queues build tasks for multi-threaded execution, includes dependency management */
class BuildQueue extends BuildManager {
  private val queued: ConcurrentLinkedDeque[QueuedTask] = new ConcurrentLinkedDeque[QueuedTask]
  private var blocked: List[QueuedTask] = Nil
  private var cycleCheck: Set[BuildDependency] = Set.empty

  /** all tasks currently in the queue */
  val alreadyQueued: mutable.HashMap[BuildDependency, QueuedTask] = new mutable.HashMap[BuildDependency, QueuedTask]
  /** all tasks that were built (successfully or permanently-failing) since the last time the queue was empty */
  val alreadyBuilt: mutable.HashMap[BuildDependency, BuildResult] = new mutable.HashMap[BuildDependency, BuildResult]
  var finishedBuilt: Queue[(BuildDependency, BuildResult)] = Queue[(BuildDependency, BuildResult)]()
  var currentQueueTask: Option[QueuedTask] = None
  /** the catalog from (logical) resource dependency to build dependency */
  val catalog: mutable.HashMap[ResourceDependency, BuildDependency] = new mutable.HashMap[ResourceDependency, BuildDependency]

  private var continue: Boolean = true
  private var pause: Boolean = false
  private var stopOnEmpty: Boolean = false
  private var finallyUnblocking = false

  val sleepTime: Int = 2000

  private def addTask(up: Update, qt: QueuedTask) {
    qt.updatePolicy = up
    log("added:" + qt.toJString)
    val qtDep = qt.task.asDependency
    qt.willProvide.foreach(rd => if (catalog.contains(rd)) log(rd.toJString + " in " + catalog(rd).toJString)
    else catalog(rd) = qtDep)
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
        // high priority: queue again but adjust updatePolicy
        val qt2 = alreadyQueued(qtDep)
        qt.merge(qt2)
        queued.remove(qt2)
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
  private def getTopTask: List[Dependency] = synchronized {
    currentQueueTask = Option(queued.poll)
    currentQueueTask.foreach(qt => alreadyQueued -= qt.task.asDependency)
    currentQueueTask match {
      case None => Nil
      case Some(qt) => qt.missingDeps.filter {
        case PhysicalDependency(file) => !file.exists
        case _ => true
      }
    }
  }

  private def getNextTask: Option[QueuedTask] = {
    val currentMissingDeps = getTopTask
    val (bDeps, rDeps) = currentMissingDeps.partition {
      case bd: BuildDependency => true
      case _ => false
    }
    val rDeps1 = rDeps.map { rd => (rd, rd match {
      case rs: LogicalDependency => catalog.get(rs)
      case _ => None
    })
    }
    val (lDeps1, fDeps1) = rDeps1.partition(_._2.isDefined)
    val fDeps = fDeps1.map(_._1)
    val lDeps = lDeps1.map(_._2.get)
    val bds = lDeps ++ bDeps.collect { case bd: BuildDependency => bd }
    if (currentMissingDeps.nonEmpty) {
      val qt = currentQueueTask.get // is non-empty if deps are missing
      currentQueueTask = None
      if (fDeps.nonEmpty) {
        qt.missingDeps = fDeps
        log("blocked: " + qt.toJString)
        blocked = blocked ::: List(qt)
        getNextTask
      } else {
        qt.missingDeps = Nil
        queued.addFirst(qt)
        cycleCheck += qt.task.asDependency
        bds.foreach(t => buildDependency(qt.updatePolicy.forDependencies, t))
        getNextTask
      }
    } else currentQueueTask
  }

  /* not used yet - logical dependencies resolved in getNextTask via catalog, file dependencies not resolved yet */
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

  /** adds tasks for all dependencies of a task (given as a [[BuildDependency]])
    *
    * @param up the update level for the dependency
    * @param bd build dependency to be added
    */
  private def buildDependency(up: Update, bd: BuildDependency) = if (!cycleCheck.contains(bd)) {
    val tar = bd.getTarget(controller)
    val inFile = bd.archive / tar.inDim / bd.inPath
    val bt = bd match {
      case _: FileBuildDependency => tar.makeBuildTask(bd.archive, bd.inPath)
      case dbd: DirBuildDependency => tar.makeBuildTask(dbd.archive, dbd.inPath, dbd.children)
    }
    val qt = new QueuedTask(tar, bt)
    qt.lowPriority = false
    qt.dependencyClosure = true
    addTask(up, qt)
  }

  /** unblock previously blocked tasks whose dependencies have now been provided */
  private def unblockTasks(res: BuildResult) {
    blocked.foreach { bt =>
      bt.missingDeps = bt.missingDeps diff res.provided
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
    controller.extman.removeExtension(serve)
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
        if (pause) Thread.sleep(sleepTime)
        else {
          getNextTask match {
            case Some(qt) =>
              // TODO run this in a Future and track dependencies
              val res1 = qt.target.checkOrRunBuildTask(qt.neededDeps.toSet, qt.task, qt.updatePolicy)
              val res = res1 match {
                // let's assume for now that the estimation is better than the actual result
                case BuildSuccess(u, Nil) => BuildSuccess(u, qt.willProvide)
                case _ => res1
              }
              val qtDep = qt.task.asDependency
              if (!qt.dependencyClosure) cycleCheck -= qtDep
              synchronized {
                currentQueueTask = None
                /* add two dummy results into the finished queue to show what happened to a blocked task */
                if (qt.forceRun.nonEmpty) {
                  finishedBuilt +:=(qtDep, BuildEmpty("was blocked"))
                  finishedBuilt +:=(qtDep, MissingDependency(qt.forceRun, qt.willProvide))
                }
                finishedBuilt +:=(qtDep, res)
              }
              if (finishedBuilt.length > 200) {
                finishedBuilt = finishedBuilt.dropRight(100)
              }
              res match {
                case _: BuildSuccess | _: BuildFailure | _: BuildEmpty =>
                  // remember finished build
                  if (!alreadyBuilt.isDefinedAt(qtDep)) {
                    alreadyBuilt(qtDep) = res
                  }
                  res.provided.foreach(catalog(_) = qtDep)
                // TODO write file errors/.../file.deps
                // XML file containing used, provided, had errors
                case MissingDependency(missing, provided) =>
                  // register missing dependencies and requeue
                  qt.missingDeps = missing
                  if (!finallyUnblocking) blocked = blocked ::: List(qt)
              }
              unblockTasks(res)
            case None =>
              if (blocked.nonEmpty) {
                log("flush blocked tasks by ignoring their missing dependencies")
                finallyUnblocking = true
                val qt = blocked.head
                qt.forceRun = qt.missingDeps
                qt.missingDeps = Nil
                blocked = blocked.tail
                queued.add(qt)
              }
              else if (stopOnEmpty)
                continue = false
              else {
                if (currentQueueTask.isEmpty) {
                  cycleCheck = Set.empty
                  alreadyBuilt.clear
                  finallyUnblocking = false
                }
                Thread.sleep(sleepTime)
              }
          }
        }
      }
    }
  }

  def waitToEnd: Unit = {
    destroyWhenQueueEmpty
    buildThread.join()
  }

  def getQueueInfo: JSON = synchronized {
    val qSize = queued.size
    val iter = queued.iterator.asScala
    val num = 48
    val firsts = (if (qSize > num + 12) iter.take(num) else iter).toList.map(_.toJson)
    val hasMore = iter.hasNext
    val rest = if (hasMore) firsts :+ JSONString("and " + (qSize - num) + " more ...") else firsts
    val q = currentQueueTask.toList.map(q => JSONString("running: " + q.toJString)) ++ rest
    val bs = blocked.map(_.toJson)
    val fs = finishedBuilt.map {
      case (d, r) =>
        JSONObject("dependency" -> d.toJson, "result" -> r.toJson)
    }
    JSONObject("count" -> JSONInt(qSize),
      "queue" -> JSONArray(q: _*),
      "blocked" -> JSONArray(bs: _*),
      "finished" -> JSONArray(fs: _*))
  }

  def getTraversingBuildTargetExtensions: List[String] =  "mmt-omdoc" ::
    controller.getConfig.getEntries(classOf[ExtensionConf]).collect {
      case ExtensionConf(key, cls, args) if classOf[BuildTarget].isAssignableFrom(Class.forName(cls))
      => key
    }

  def getArchives: List[String] =
    controller.backend.getArchives.map(a => a.archString)

  /** serves lists of [[Error]]s */
  private val serve = new ServerExtension("queue") {
    def apply(path: List[String], query: String, body: Body, session: Session) = path match {
      case List("clear") =>
        finishedBuilt = Queue.empty
        blocked = Nil
        queued.clear
        alreadyBuilt.clear
        alreadyQueued.clear
        cycleCheck = Set.empty
        Server.JsonResponse(JSONNull)
      case List("pause") =>
        pause = !pause
        Server.JsonResponse(JSONBoolean(pause))
      case List("targets") =>
        Server.JsonResponse(JSONArray(getTraversingBuildTargetExtensions.map(JSONString): _*))
      case List("archives") =>
        Server.JsonResponse(JSONArray(getArchives.map(JSONString): _*))
      case _ =>
        Server.JsonResponse(getQueueInfo)
    }
  }
}
