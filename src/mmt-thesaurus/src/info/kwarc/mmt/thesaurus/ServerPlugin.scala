package info.kwarc.mmt.thesaurus

import info.kwarc.mmt.api._
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend._
import info.kwarc.mmt.api.documents._
import info.kwarc.mmt.api.presentation._
import info.kwarc.mmt.api.notations._
import info.kwarc.mmt.api.backend._
import info.kwarc.mmt.api.ontology._
import info.kwarc.mmt.api.informal._
import info.kwarc.mmt.api.modules.DeclaredTheory
import info.kwarc.mmt.stex._
import symbols.{Constant, FinalConstant}
import utils._

import scala.collection.mutable.ArrayBuffer
//import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._
import org.apache.commons.lang3.StringEscapeUtils.{unescapeJava}

case class ThesaurusError(text : String) extends Error(text)


class ThesaurusPlugin extends ServerExtension("thesaurus") with Logger {
  
  override val logPrefix = "thesaurus"
     /** Server */
  def apply(uriComps: List[String], query: String, body : Body, session: Session): HLet = {
    lazy val json = body.asJSON match {
      case j: JSONObject => j
      case _ => throw ServerError("body must be json object")
    }
    
    try {
      uriComps match {
        case "getNotations" :: _ => getNotations(json)
        case "getDefinitions" :: _ => getDefinitions(json)
        case "getAllEntries" :: _ => getAllEntries(json)
        case "getEntry" :: _ => getEntry(json)
        case "getTranslations" :: _ => getTranslations(json)
        case "getHypernyms" :: _ => getHypernyms(json)
        case "getHyponyms" :: _ => getHyponyms(json)
        case "getSynonyms" :: _ => getSynonyms(json)
        case _ => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new ThesaurusError("Invalid Request" + uriComps)))
       }
    } catch {
      case e : Error => 
        log(e.shortMsg) 
        errorResponse(e.shortMsg, List(e))
      case e : Exception => 
        errorResponse("Exception occurred : " + e.getStackTrace(), List(e))
    }
  }
  
  def getNotations(params: JSONObject) = {
      val spath = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
      val languageO = params("language").map(_.toString).map(removeQuotes(_))
      val dimensionO = params("dimension").map(_.toString).map(removeQuotes(_))

      controller.get(spath) match {
        case c : Constant =>
          var notations = dimensionO match {
            case None => c.notC.getAllNotations
            case Some("parsing") => c.notC.parsingDim.notations.values.flatten
            case Some("presentation") => c.notC.presentationDim.notations.values.flatten
            case Some("verbalization") => c.notC.verbalizationDim.notations.values.flatten
            case Some(s) => throw ServerError("Invalid notation dimension: '" + s  + "'. Expected parsing, presentation or verbalization")
          }
          
          notations = languageO match {
            case None => notations
            case Some(lang) => notations.filter(_.scope.languages.contains(lang))
          }
          Server.JsonResponse(JSONArray(notations.map(n => JSONArray(toStringMarkers(n).map(s => JSONString(s)) : _*)).toSeq :_*))
        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
      }
  }
  
  private def toStringMarkers(not : TextNotation) : List[String] = {
   not.parsingMarkers flatMap {
      case a : Arg => Some("_")
      case a : SeqArg => Some("_...")
      case a : ImplicitArg => None
      case d : Delimiter => Some(d.text)
      case v : Var => Some("_")
      case _ => None
    }
  }
  
  private def getDefinitions(params: JSONObject) = {
      val path = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
      val spath = controller.get(path) match {
        case c: Constant =>
          c.path
        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
      }
      val language = params("language").map(_.toString).map(removeQuotes(_)).getOrElse("en")
      Server.JsonResponse(JSONString(ThesaurusGenerator.getDefinitions(spath, language)))
  }

  private def getTranslations(params: JSONObject) = {
    val spath = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
    val languageO = params("language").map(_.toString).map(removeQuotes(_))

    val responseBuffer = ArrayBuffer.empty[JSON]

    controller.get(spath) match {
      case c : Constant =>
        var notations = c.notC.verbalizationDim.notations.values.flatten
        notations = languageO match {
          case None => notations
          case Some(lang) => notations.filter(!_.scope.languages.contains(lang))
        }

        notations foreach { n =>
          val response = new collection.mutable.HashMap[String, JSON]()
          response("language") = JSONString(n.scope.languages.head)
          response("notation") = JSONString(toStringMarkers(n).mkString(" "))
          responseBuffer += JSONObject(response.toSeq: _*);
        }
      case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
    }

    Server.JsonResponse(JSONArray(responseBuffer : _*))
  }

  private def getHypernyms(params:JSONObject) = {
    val path: Path = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
    val language = params("language").map(_.toString).map(removeQuotes(_)).getOrElse("en")

    val responseBuffer = ArrayBuffer.empty[JSON]

    val spath = controller.get(path) match {
      case c: Constant =>
        c.path
      case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
    }

    val hypernyms = ThesaurusGenerator.getHypernyms(spath, language)

    hypernyms foreach { n =>
      val response = new collection.mutable.HashMap[String, JSON]()
      response("language") = JSONString(n.scope.languages.head)
      response("notation") = JSONString(toStringMarkers(n).mkString(" "))
      responseBuffer += JSONObject(response.toSeq: _*);
    }

    Server.JsonResponse(JSONArray(responseBuffer : _*))
  }


  private def getHyponyms(params:JSONObject) = {
    val path: Path = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
    val language = params("language").map(_.toString).map(removeQuotes(_)).getOrElse("en")

    val responseBuffer = ArrayBuffer.empty[JSON]

    val spath = controller.get(path) match {
      case c: Constant =>
        c.path
      case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
    }

    val hypernyms = ThesaurusGenerator.getHyponyms(spath, language)

    hypernyms foreach { n =>
      val response = new collection.mutable.HashMap[String, JSON]()
      response("language") = JSONString(n.scope.languages.head)
      response("notation") = JSONString(toStringMarkers(n).mkString(" "))
      responseBuffer += JSONObject(response.toSeq: _*);
    }

    Server.JsonResponse(JSONArray(responseBuffer : _*))
  }

  private def getSynonyms(params : JSONObject) = {
    val path: Path = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
    val language = params("language").map(_.toString).map(removeQuotes(_)).getOrElse("en")

    val responseBuffer = ArrayBuffer.empty[JSON]

    val spath = controller.get(path) match {
      case c: Constant =>
        c.path
      case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
    }

    val synonyms = ThesaurusGenerator.getSynonyms(spath, language)

    synonyms foreach { n =>
      val response = new collection.mutable.HashMap[String, JSON]()
      response("language") = JSONString(n.scope.languages.head)
      response("notation") = JSONString(toStringMarkers(n).mkString(" "))
      responseBuffer += JSONObject(response.toSeq: _*);
    }

    Server.JsonResponse(JSONArray(responseBuffer : _*))
  }

  private def getAllEntries(json: JSONObject) = {
    val language = json("lang").map(_.toString).map(removeQuotes(_)).getOrElse("en")

    val page_number : Int = json("page_number") match {
      case Some(n:JSONInt) => n.value
      case Some(d:JSONFloat) => d.value.toInt
      case _ => 1
    }

    val entries_per_page : Int = json("entries") match {
      case Some(n:JSONInt) => n.value
      case Some(d:JSONFloat) => d.value.toInt
      case _ => 1
    }

    ThesaurusGenerator.setControllerAndPresenter(controller)
    ThesaurusGenerator.loadAllEntries(controller)

    Server.JsonResponse(ThesaurusGenerator.getAllEntries(language, page_number, entries_per_page))
  }

  private def getEntry(params: JSONObject) = {
    val path: Path = Path.parse(unescapeJava(removeQuotes(params("spath").getOrElse(throw ServerError("No spath found")).toString)))
    val language = params("language").map(_.toString).map(removeQuotes(_)).getOrElse("en")
    val spath = controller.get(path) match {
      case c: Constant =>
        c.path
      case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass)
    }
    Server.JsonResponse(ThesaurusGenerator.getEntry(spath, language))
  }
  
  //utils
  private def errorResponse(text : String, errors : List[Throwable]) : HLet = {
    JsonResponse("", s"MMT Error in Thesaurus extension: $text ", errors)
  }
  
  private def JsonResponse(content : String, info : String, errors : List[Throwable]) : HLet = {
    val response = new collection.mutable.HashMap[String, JSON]()
    response("content") = JSONString(content)
    if (errors == Nil) { //no errors
      val status = new collection.mutable.HashMap[String, JSON]()
      status("conversion") = JSONInt(0) //success
      val messages = new collection.mutable.HashMap[String, JSON]()
      if (info != "") {
        val message = new collection.mutable.HashMap[String, JSON]()
        message("type") = JSONString("Info")
        message("shortMsg") = JSONString(info)
        message("longMsg") = JSONString(info)
        //no srcref
        messages("0") = JSONObject(message.toSeq : _*)
      }
      status("messages") = JSONObject(messages.toSeq : _*)
      response("status") = JSONObject(status.toSeq : _*)
    } else { //there are errors
      val status = new collection.mutable.HashMap[String, JSON]()
      if (content == "") {
        status("conversion") = JSONInt(2) //failed with errors
      } else {
        status("conversion") = JSONInt(2) //success with errors
      }
      val messages = new collection.mutable.HashMap[String, JSON]()
      errors.zipWithIndex foreach { p => 
        val message = new collection.mutable.HashMap[String, JSON]()
        p._1 match {
          case se : SourceError =>
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(se.mainMessage)
            message("longMsg") = JSONString(se.getStackTrace.mkString("\n"))
            message("srcref") = JSONObject(List("from" -> JSONObject(List("line" -> JSONInt(se.ref.region.start.line), "col"-> JSONInt(se.ref.region.start.column)) : _*),
                                 "to" -> JSONObject(List("line" -> JSONInt(se.ref.region.end.line), "col" -> JSONInt(se.ref.region.end.column)) : _*)) : _*)
          case e =>
            message("type") = JSONString("Fatal")
            message("shortMsg") = JSONString(e.getMessage)
            message("longMsg") = JSONString(e.getStackTrace.mkString("\n"))
            //no srcref :(
          }
          messages(p._2.toString) = JSONObject(message.toSeq : _*)
      }
      status("messages") = JSONObject(messages.toSeq : _*)
      response("status") = JSONObject(status.toSeq : _*)
    }
      log("Sending Response: " + response)
      Server.JsonResponse(JSONObject(response.toSeq : _*))
  }

  private def removeQuotes(s:String):String = {
    s.substring(1, s.length - 1)
  }
}
