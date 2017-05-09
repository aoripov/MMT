package info.kwarc.mmt.thesaurus

import info.kwarc.mmt.api._
import modules._
import symbols._
import documents._
import presentation._
import frontend._
import libraries._
import objects._
import utils._
import informal._
import archives._
import notations._
import scala.xml.{Node }
import ontology._
import info.kwarc.mmt.stex._

import org.apache.commons.lang3.StringEscapeUtils._
import scala.collection.mutable.ListBuffer

abstract class ThesaurusGeneratorGenericBase {

  // Generic Thesaurus API functions
  def getEntry(spath : GlobalName, lang : String) : JSON
  def getAllEntries(language: String, page_number: Int, entries_per_page: Int): JSON
  def getSynonyms(spath : GlobalName, lang : String) : Iterable[TextNotation]

  // Non-generic methods, library specific
  def getDefinitions(spath : GlobalName, lang : String) : String
  protected def loadAllEntries(controller: Controller): Unit
}

abstract class ThesaurusGeneratorGeneric extends ThesaurusGeneratorGenericBase {

  private var counter = 0
  protected var constantPairs: Iterable[(GlobalName, TextNotation)] = new ListBuffer[(GlobalName, TextNotation)]

  def getNewId : String = {
    this.counter += 1
    this.counter.toString
  }

  def resetId() : Unit = {
    this.counter = 0
  }

  protected var presenter: ThesaurusFormatter = null
  protected var controller: Controller = null
  protected var rh: StringBuilder = null

  def setControllerAndPresenter(controller: Controller): Unit

  def loadAllEntries(controller: Controller): Unit

  def getDefinitions(spath : GlobalName, lang : String) : String

  def getSynonyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = {
    val constant = controller.library.getConstant(spath)
    constant.notC.verbalizationDim.get(lang=Some(lang))
  }

  def getEntry(spath : GlobalName, lang : String) : JSON = {
    val constant = controller.library.getConstant(spath)
    val verbs = if (constant.notC.verbalizationDim.isDefined) {
      constant.notC.verbalizationDim.notations.values.flatten.map(constant.path -> _)
    } else null
    JSONArray.fromList(getPresentationAsJSONs(verbs, lang).toList)
  }

  def getAllEntries(language: String, page_number: Int, entries_per_page: Int): JSON = {
    this.resetId()
    JSONArray.fromList(getPresentationAsJSONs(getByPage(getByLanguage(this.constantPairs, language), page_number, entries_per_page),language).toList)
  }

  protected def getByPage(verbs: Iterable[(GlobalName, TextNotation)], page_number: Int, entries_per_page: Int) : Iterable[(GlobalName, TextNotation)] = {
    verbs.slice((page_number - 1) * entries_per_page, page_number * entries_per_page)
  }

  protected def getByLanguage(verbs: Iterable[(GlobalName, TextNotation)], language: String): Iterable[(GlobalName, TextNotation)] = {
    val out = new ListBuffer[(GlobalName, TextNotation)]()
    verbs foreach {p => p._2.scope.languages.foreach { lang =>
      if (lang == language) {
        out.append(p)
      }
    }}
    out.toList
  }

  protected def getPresentationAsJSONs(verbs : Iterable[(GlobalName, TextNotation)], language : String): Iterable[JSON] = {
    val out = verbs map(x => this.getPresentationAsJSON(x._1, x._2, language))
    out
  }

  protected def getPresentationAsJSON(spath : GlobalName, not: TextNotation, lang : String): JSON = {
    val constant = controller.library.getConstant(spath)

    val out = new collection.mutable.HashMap[String, JSON]()

    out("id") = JSONString(this.getNewId)
    out("spath") = JSONString(spath.toString)

    var sb = new StringBuilder
    presenter.setRh(sb)
    presenter.doNotationRendering(spath, not)
    out("primary") = JSONString(sb.get)
    out("definition") = JSONString(getDefinitions(spath, lang))

//    println(lang + " " + JSONObject(out.toSeq : _*))
    JSONObject(out.toSeq : _*)
  }

  protected def makeString(not : TextNotation) : String = {
    val smks = not.markers map {
      case d : Delimiter => d.text
      case m => m.toString
    }
    smks.mkString(" ")
  }
}

sealed class SMGLOMThesaurusGenerator extends ThesaurusGeneratorGeneric  {

  def setControllerAndPresenter(controller: Controller): Unit = {
    this.presenter = controller.extman.get(classOf[Presenter], "thesaurus") match {
      case Some(p: ThesaurusFormatter) => p
      case _ => throw new Exception("Expected thesaurus formatter to be loaded")
    }
    this.controller = controller
  }

  def loadAllEntries(controller: Controller) : Unit = {
    if (this.constantPairs.isEmpty) {
      val mpaths = controller.depstore.getInds(ontology.IsTheory).toList
      val modules = mpaths flatMap { p =>
        try {
          controller.get(p) match {
            case d: DeclaredTheory => Some(d)
            case _ => None
          }
        } catch {
          case e: Error => None
          case e: Exception => None
        }
      }

      val verbs = modules collect {
        case thy: DeclaredTheory =>
          thy.getDeclarations collect {
            case c: Constant if c.notC.verbalizationDim.isDefined =>
              c.notC.verbalizationDim.notations.values.flatten.map(c.path -> _)
          }
      }

      val temp = verbs.flatten.flatten.distinct
      this.constantPairs = temp.sortWith((x,y) => makeString(x._2).toLowerCase() < makeString(y._2).toLowerCase())
    }
  }

  def getHypernyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = this.getNymsByRelations(spath, lang, ToObject(IsHypernymOf)).flatten

  def getHyponyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = this.getNymsByRelations(spath, lang, ToSubject(IsHypernymOf)).flatten

  protected def getVerbalizations(path: MPath, lang: String) : Iterable[TextNotation] = {
    val primarySyms = controller.depstore.getInds(IsPrimarySymbol)
    val primarySymO = primarySyms.find {
      case p : GlobalName => p.module == path
      case _ => false
    }

    primarySymO.map(controller.get) match {
      case Some(c:Constant) => c.notC.verbalizationDim.get(lang=Some(lang))
      case _ => Nil
    }
  }

  protected def getNymsByRelations(spath : GlobalName, lang:String, relEx : RelationExp): Iterable[Iterable[TextNotation]] = {
    //val primarySym = controller.depstore.queryList(spath, HasType(IsPrimarySymbol))
    lazy val mpath = spath.module
    // if (primarySym.nonEmpty) {
    controller.depstore.queryList(mpath, relEx) flatMap {
      case p: Path =>
        controller.getO(p) match {
          case Some(fd: Theory) =>
            val mpath = fd.path
            val verbs = getVerbalizations(mpath, lang)
            if (verbs.isEmpty) {
              None
            } else {
              Some(verbs)
            }
          case _ => None
        }
    }
    //  } else Nil
  }

  def getDefinitions(spath : GlobalName, lang : String) : String= {
    val defs = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy))
    val definitions = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy)) flatMap {
      case p: Path =>
        controller.getO(p) match {
          case Some(fd: Constant) =>
            val mpath = fd.home.toMPath
            if (sTeX.getLanguage(mpath).contains(lang)) {
              Some(fd)
            } else {
              None
            }
          case _ => None
        }
    }
    val sb = new StringBuilder
    definitions foreach { constant =>
      presenter.apply(constant)(sb)
    }
    sb.get
  }
}

object ThesaurusGenerator extends SMGLOMThesaurusGenerator
