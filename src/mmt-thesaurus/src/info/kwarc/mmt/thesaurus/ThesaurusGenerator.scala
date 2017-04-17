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

object ThesaurusGenerator {

  private var counter = 0
  def getNewId : String = {
    counter += 1
    counter.toString
  }
  private var presenter: ThesaurusFormatter = null
  private var controller: Controller = null
  private var rh: StringBuilder = null

  def generate(controller: Controller, params: JSONObject): JSON = {
    this.controller = controller
    this.presenter = controller.extman.get(classOf[Presenter], "thesaurus") match {
      case Some(p: ThesaurusFormatter) => p
      case _ => throw new Exception("Expected thesaurus formatter to be loaded")
    }

    println(params.toString)

    val page_number : Int = params("page_number") match {
      case Some(n:JSONInt) => n.value
      case Some(d:JSONFloat) => d.value.toInt
      case _ => 1
    }

    val entries : Int = params("entries") match {
      case Some(n:JSONInt) => n.value
      case Some(d:JSONFloat) => d.value.toInt
      case _ => 1
    }

    val mpaths = controller.depstore.getInds(ontology.IsTheory).toList
    val modules = mpaths flatMap { p =>
      try {
        controller.get(p) match {
          case d : DeclaredTheory => Some(d)
          case _ =>  None
        }
      } catch {
        case e : Error => None
        case e : Exception => None
      }
    }

    val verbs = modules collect {
      case thy: DeclaredTheory =>
        thy.getDeclarations collect {
          case c: Constant if c.notC.verbalizationDim.isDefined =>
            c.notC.verbalizationDim.notations.values.flatten.map(c.path -> _)
        }
    }

    var theories: List[(GlobalName, TextNotation)] = verbs.flatten.flatten.distinct

    counter = 0 // reset counter for each request

    var language = params("lang").getOrElse("\"en\"").toString // english is default
    language = language.substring(1, language.length - 1)
    theories = theories.sortWith((x,y) => makeString(x._2).toLowerCase() < makeString(y._2).toLowerCase())

    var verbs_lang = getByLanguage(theories, language)
    verbs_lang = verbs_lang.slice((page_number - 1) * entries, page_number * entries)

    val out = present(verbs_lang, language)
    JSONArray.fromList(out)
  }

  private def filter(verbs: Iterable[(GlobalName, TextNotation)]): Iterable[(GlobalName, TextNotation)] = {
    verbs
  }

  // easy-to-use HTML markup
  protected val htmlRh = utils.HTML(s => rh(s))
  import htmlRh._

  private def makeString(not : TextNotation) : String = {
    val smks = not.markers map {
      case d : Delimiter => d.text
      case m => m.toString
    }
    smks.mkString(" ")
  }

  private def getByLanguage(verbs: Iterable[(GlobalName, TextNotation)], language: String): List[(GlobalName, TextNotation)] = {
    val out = new ListBuffer[(GlobalName, TextNotation)]()
    verbs foreach {p => p._2.scope.languages.foreach { lang =>
      if (lang == language) {
        out.append(p)
      }
    }}
    out.toList
  }

  private def present(verbs : Iterable[(GlobalName, TextNotation)], language : String): List[JSON] = {
    val items = new collection.mutable.HashMap[String, List[(GlobalName, TextNotation)]]
    verbs foreach {p => p._2.scope.languages.foreach { lang =>
     if (lang != "") {
       if (!items.contains(lang)) {
         items(lang) = Nil
       }
       items(lang) ::= p
     }
    }}

    val out = verbs map(x => this.present(language, x._1, x._2))

    out.filter(x => x != null).toList
  }
  
  private def getVerbalizations(path: MPath, lang: String) : List[TextNotation] = {
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

  private def getNymsByRelations(spath : GlobalName, lang:String, relEx : RelationExp): Iterable[Iterable[TextNotation]] = {
    val primarySym = controller.depstore.queryList(spath, HasType(IsPrimarySymbol))
    lazy val mpath = spath.module
    if (primarySym.nonEmpty) {
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
    } else Nil
  }

  def getHypernyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = getNymsByRelations(spath, lang, ToObject(IsHypernymOf)).flatten

  def getHyponyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = getNymsByRelations(spath, lang, ToSubject(IsHypernymOf)).flatten

  def getSynonyms(spath : GlobalName, lang : String) : Iterable[TextNotation] = {
    val constant = controller.library.getConstant(spath)
    constant.notC.verbalizationDim.get(lang=Some(lang))
  }

  private def present(lang : String,  spath : GlobalName, not: TextNotation): JSON = {
    val constant = controller.library.getConstant(spath)
    val doc = spath.doc
    val mod = spath.module.name
    val name = spath.name


    val notations = (constant.notC.parsingDim.notations.values.flatten ++ constant.notC.presentationDim.notations.values.flatten).map(n => spath -> n).toList.distinct

    val definitions = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy)) flatMap {
          case p: Path =>
            controller.getO(p) match {
              case Some(fd: Constant) =>
                val mpath = fd.home.toMPath
                if (sTeX.getLanguage(mpath) contains lang) {
                  Some(fd)
                } else {
                  None
                }
              case _ => None
            }
        }

    val response = new collection.mutable.HashMap[String, JSON]()

    constant.notC.verbalizationDim.get(lang=Some(lang)).filter(_ != not)

    if (definitions.isEmpty) {
      return null
    }

    val out = new collection.mutable.HashMap[String, JSON]()

    out("id") = JSONString(getNewId)

    out("spath") = JSONString(spath.toString)

    var sb = new StringBuilder
    presenter.setRh(sb)
    presenter.doNotationRendering(spath, not)
    out("primary") = JSONString(sb.get)

    sb = new StringBuilder
    presenter.setRh(sb)
    definitions foreach { constant =>
      presenter.apply(constant)(sb)
    }
    out("definition") = JSONString(sb.get)

    JSONObject(out.toSeq : _*)
  }

  private def removeQuotes(s:String):String = {
    s.substring(1, s.length - 1)
  }
}
