package info.kwarc.mmt.dictionary

import info.kwarc.mmt.api.{SourceError, _}
import info.kwarc.mmt.api.web._
import info.kwarc.mmt.api.frontend.Logger
import info.kwarc.mmt.api.utils._
import tiscaf.HLet

case class DictionaryError(text : String) extends Error(text)

class DictionaryServerPlugin  extends ServerExtension("dictionary") with Logger {

  override val logPrefix = "dictionary"
  /** Server */
  def apply(uriComps: List[String], query: String, body : Body, session: Session): HLet = {
    lazy val json = body.asJSON match {
      case j: JSONObject => j
      case _ => throw ServerError("Body must be json object")
    }

    try {
      uriComps match {
        case "getTranslation" :: _ => getTranslation(json)
        case "getSuggestions" :: _ => getSuggestions(json)
        case _ => errorResponse("Invalid request: " + uriComps.mkString("/"), List(new DictionaryError("Invalid Request" + uriComps)))
      }
    } catch {
      case e : Error =>
        log(e.shortMsg)
        errorResponse(e.shortMsg, List(e))
      case e : Exception =>
        errorResponse("Exception occurred : " + e.getStackTrace, List(e))
    }
  }

  private def getTranslation(param: JSONObject) = {
    val from = param("from").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found from"))
    val to = param("to").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found to"))
    val q = param("q").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found query"))

    SMGloMDictionary.setController(controller)
    SMGloMDictionary.loadIndex()

    val translations = SMGloMDictionary.findTranslations(from, to, q)
    val out = translations match {
      case Some(a)  => {
        val out = new collection.mutable.HashMap[String, JSON]()
        val trans = a._2 map (s => JSONString(s))
        out("trans") = JSONArray.fromList(trans)
        out("p") = JSONString(a._1.toPath)
        out("t") = JSONString(a._3)
        JSONObject(out.toSeq: _*)
    }}

    Server.JsonResponse(out)
  }

  private def getSuggestions(param: JSONObject) = {
    val from = param("from").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found from"))
    val to = param("to").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found to"))
    val q = param("q").map(_.toString).map(removeQuotes(_)).getOrElse(throw ServerError("Not found query"))

    SMGloMDictionary.setController(controller)
    SMGloMDictionary.loadIndex()

    val translations = SMGloMDictionary.findByPrefix(from, to, q)
    val out = translations map (a => JSONString(a._3))

    Server.JsonResponse(JSONArray.fromList(out.toList))
  }

  private def errorResponse(text : String, errors : List[Throwable]) : HLet = {
    JsonResponse("", s"MMT Error in Dictionary extension: $text", errors)
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