package info.kwarc.mmt.api.frontend

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.archives._
import utils._
import scala.collection.immutable.{ListMap}

/** an entry in an MMT configuration file (.cfg) */
abstract class ConfEntry {
   /** archive id, build target key, etc. */
   val id : String
} 

/**
 * registers [[BuildTarget]]s with their arguments
 *
 * @param cls the qualified Java class name of this target's implementation
 *  (must only be on the class path if this target is actually used)
 * @param key the key of the target
 * @param args the arguments to be used if this target is instantiated
 */
case class TargetConf(cls : String, key : String, args : List[String]) extends ConfEntry {
   val id = key
}

/**
 * registers an archive with its formats
 */
case class ArchiveConf(id : String, formats : List[String]) extends ConfEntry

/** defines an archive format
 *  @param id the format name
 *  @param importers the importers to be used for archives of this format
 *  @param exporters the exporters to be used for archives of this format
 */
case class FormatConf(id : String, importers : List[String], exporters : List[String]) extends ConfEntry

/**
 * an MMT configuration stores catalogs for finding extensions, archives, etc.
 * It is a list of [[MMTConfEntry]] that can be read from a .cfg file
 */
class MMTConfig {
    private var base : String = ""
    private var entries: List[ConfEntry] = Nil

    def addEntry(e: ConfEntry) {entries = entries ::: List(e)}
    def setBase(b : String) {
      base = b
    }

    def add(that: MMTConfig) = {
       that.getEntries foreach addEntry
       setBase(that.getBase)
    }
    
    def getBase = base

    def getEntries() = entries
    def getEntries[E <: ConfEntry](cls: Class[E]): List[E] = entries.collect {
       case e: E@unchecked if cls.isInstance(e) => e
    }
    def getEntry[E <: ConfEntry](cls: Class[E], id: String): E = getEntries(cls).find {e =>
       e.id == id
    }.getOrElse {
       throw ConfigurationError(id)
    }

    def getArchive(aid : String) = getEntry(classOf[ArchiveConf], aid)
    def getArchives = getEntries(classOf[ArchiveConf])
    def getFormat(id: String) = getEntry(classOf[FormatConf], id)
    
    def getImporters(format : String) = getFormat(format).importers
    def getExporters(format : String) = getFormat(format).exporters
    def getImportersForArchive(archive : String) = getArchive(archive).formats.flatMap(getImporters).distinct
    def getExportersForArchive(archive : String) = getArchive(archive).formats.flatMap(getExporters).distinct
    
    
    def loadAllArchives(controller: Controller) {
       getArchives foreach { arch => 
        controller.handle(AddArchive(File(base + arch.id)))
      }
    }
    
    def loadAllNeededTargets(controller: Controller) {
      val archives = getArchives
      val activeFormats = archives.flatMap(_.formats).distinct.map {id => 
        getEntries(classOf[FormatConf]).find(_.id == id).getOrElse(throw new Exception("Unknown format id: " + id))
      }
      
      val activeTargets = activeFormats.flatMap(f => f.importers ::: f.exporters).distinct.map {key => 
        getEntry(classOf[TargetConf], key)
      }
      
      activeTargets foreach {comp => 
        println("loading " + comp.cls)
        controller.handle(AddExtension(comp.cls, comp.args))
      }
    }
}


object MMTConfig {
  /**
   * parses a configuration file
   * 
   * syntax:
   *  configuration includes: #include PATH/TO/CONF/FILE
   *  section headers: #targets | #formats | #archives
   *  all other lines are configuration entries of the respective section
   */
  def parse(f: File) : MMTConfig = {
    val config = new MMTConfig
    var section = ""
    File.ReadLineWise(File(f)) {l =>
      val line = l.trim
      if (line.startsWith("//") || line.isEmpty) {
        //ignore
      } else if (line.startsWith("#include")) {
         val inc = line.substring("#include".length)
         val incConf = parse(File(inc))
         config.add(incConf)
      } else if (line.startsWith("#")) {
        section = line.substring(1)
      } else section match {
        // TODO "importers" and "exporters" are deprecated but still used by Mihnea
        case "importers" | "exporters" | "targets" => line.split("\\s+").toList match {
          case key :: cls :: args =>
            config.addEntry(TargetConf(cls, key, args))
          case _ => println(s"Invalid target line: $line")
        }
        case "archives" => line.split(" ").toList match {
          case id :: fmtsS :: Nil =>
            val fmts = fmtsS.split(",").toList
            config.addEntry(ArchiveConf(id, fmts))
          case _ => println("Invalid archives line: `" + line + "`")
        }
        case "formats" => line.split(" ").toList match {
          case id :: impsS :: expsS :: Nil =>
            val imps = impsS.split(",").toList
            val exps = expsS.split(",").toList
            config.addEntry(FormatConf(id, imps, exps))
          case _ => println("Invalid formats line: `" + line + "`")
        }
        case "base" => config.setBase(line)
        case _ => println("ignoring invalid section: `" + section + "`")
      }
    }
    config
  }
}