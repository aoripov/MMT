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

object ThesaurusGenerator {

  private var counter = 0
  def getNewId : String = {
    counter += 1
    counter.toString
  }
  private var presenter: ThesaurusFormatter = null
  private var controller: Controller = null
  private var rh: StringBuilder = null

  def generate(controller: Controller): JSON = {
    this.controller = controller
    this.presenter = controller.extman.get(classOf[Presenter], "thesaurus") match {
      case Some(p: ThesaurusFormatter) => p
      case _ => throw new Exception("Expected thesaurus formatter to be loaded")
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
//    print(mpaths)
    val verbs = modules collect {
      case thy: DeclaredTheory =>
        thy.getDeclarations collect {
          case c: Constant if c.notC.verbalizationDim.isDefined =>
            c.notC.verbalizationDim.notations.values.flatten.map(c.path -> _)
        }
    }
//    println()
//    print(verbs)
//    println()
//    print(verbs.flatten.flatten.toList.distinct)
//    println()
    val theories: List[(GlobalName, TextNotation)] = verbs.flatten.flatten.distinct
    val head = theories.filter(p => {
      println(p._1.name.toString)
      true
    })


    val out = present(verbs.flatten.flatten.distinct)
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

  private def present(verbs: Iterable[(GlobalName, TextNotation)]): List[JSON] = {
    val items = new collection.mutable.HashMap[String, List[(GlobalName, TextNotation)]]
    verbs foreach {p => p._2.scope.languages.foreach { lang =>
     if (lang != "") {
       if (!items.contains(lang)) {
         items(lang) = Nil
       }
       items(lang) ::= p
     }
    }}
    def getCls(lang: String) = if (lang == "en") "active" else ""

    val out = verbs map(x => this.present("en", x._1, x._2))
//    div(attributes = List("id" -> "glossary")) {
//      ul("nav nav-tabs") {
//        items.foreach { p =>
//          li(getCls(p._1)) {
//            rh(<a data-target={ "#gtab_" + p._1 } class="gs_tab"> { p._1 } </a>)
//          }
//        }
//      }
//      div("tab-content") {
//        items.foreach { p =>
//          div(cls = ("tab-pane " + getCls(p._1)), attributes = List("id" -> ("gtab_" + p._1))) {
//            ul("glossary") {
//              val glossary = p._2.toList.sortWith((x,y) => makeString(x._2).toLowerCase() < makeString(y._2).toLowerCase())
//              glossary.foreach(v => present(p._1, v._1, v._2))
//            }
//          }
//        }
//      }
//    }
    out.filter(x => x != null).toList
  }
  
  private def getVerbalizations(path: MPath, lang: String) : List[TextNotation] = {
    
    //getPrimarySym(path : MPath) : Option[GlobalName]
    val primarySyms = controller.depstore.getInds(IsPrimarySymbol)
    val primarySymO = primarySyms.find { 
      case p : GlobalName => p.module == path 
      case _ => false
    }
    
    //getVerbs(spath : Option[GlobalName], lang : String) : List[TextNotation]     
    primarySymO.map(controller.get) match {
      case Some(c:Constant) => c.notC.verbalizationDim.get(lang=Some(lang))
      case _ => Nil
    }
  }

  private def present(lang : String,  spath : GlobalName, not: TextNotation): JSON = {
    val doc = spath.doc
    val mod = spath.module.name
    val name = spath.name
    val constant = controller.library.getConstant(spath)
    val alternatives = constant.notC.verbalizationDim.notations.values.flatten.flatMap(_.scope.languages.filter(_ != lang)).toSet

    val notations = (constant.notC.parsingDim.notations.values.flatten ++ constant.notC.presentationDim.notations.values.flatten).map(n => spath -> n).toList.distinct

    val defs = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy)) flatMap {
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

    val primarySym = controller.depstore.queryList(spath, HasType(IsPrimarySymbol))
    val hypernyms = if (!primarySym.isEmpty) {
      val mpath = spath.module 
      controller.depstore.queryList(mpath, ToObject(IsHypernymOf)) flatMap {
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
    
    val hyponyms = if  (!primarySym.isEmpty) {
      val mpath = spath.module 
      controller.depstore.queryList(mpath, ToSubject(IsHypernymOf)) flatMap {
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
    
    print(defs.length)
    
    val synonyms = constant.notC.verbalizationDim.get(lang=Some(lang)).filter(_ != not)
    
    if (defs.isEmpty) {
      return null
    }

    var sb = new StringBuilder
    presenter.setRh(sb)
    presenter.doNotationRendering(spath, not)

    val out = new collection.mutable.HashMap[String, JSON]()
    out("primary") = JSONString(sb.get)

    sb = new StringBuilder
    presenter.setRh(sb)
    defs foreach { fd =>
      presenter.apply(fd)(sb)
    }

    out("definition") = JSONString(sb.get)
    out("id") = JSONString(getNewId)

    JSONObject(out.toSeq : _*)
  }
}
