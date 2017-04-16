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
import info.kwarc.mmt.stex._
import symbols.{Constant}

import utils._
//import scala.util.parsing.json._
import tiscaf._
import scala.concurrent._

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
      val spathS = params("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params("language").map(_.toString)
      val dimensionO = params("dimension").map(_.toString)
      
      val spath = Path.parse(spathS)
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
        case x => throw ServerError("Expected path pointing to constant, found :" + x.getClass())
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
      val spathS = params("spath").getOrElse(throw ServerError("No spath found")).toString
      val languageO = params("language").map(_.toString)
      val spath = Path.parse(spathS)
      var resultSet = controller.depstore.queryList(spath, ToObject(IRels.isDefinedBy))
      resultSet = languageO match {
        case None => resultSet
        case Some(_) => resultSet.filter(p => sTeX.getLanguage(p) == languageO)
      }
      
      //presenting
      val pres = controller.extman.get(classOf[Presenter]).find(_.isApplicable("thesaurus")).getOrElse(throw ServerError("No presenter found"))
      val resultNodes = resultSet flatMap {p => 
        controller.get(p) match {
          case s : StructuralElement =>
            val rb = new presentation.StringBuilder
            pres(s)(rb)
            Some(rb.get)
          case _ => None
        }
      }
      Server.JsonResponse(JSONArray(resultNodes.map(s => JSONString(s)).toSeq :_*))
  }

  private def getAllEntries(json: JSONObject) = {
    Server.JsonResponse(ThesaurusGenerator.generate(controller, json))
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
}