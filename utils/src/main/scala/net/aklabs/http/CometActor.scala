package net.aklabs.http

import akka.actor.{Actor, ActorSystem}
import akka.dispatch.MessageDispatcher
import com.fasterxml.jackson.databind._
import net.aklabs.helpers.JsonHelpers.{JArray, JField, JObject, JString, Jckson}
import org.pmw.tinylog.Logger

import scala.collection.mutable

case class CometUpdate()
case class CometUpdateEval(script: String)
case class CometUpdateJObj(name: String, js_map: Map[String, _])
case class CometUpdateJVal(name: String, js_value: JsonNode)
case class CometUpdateJValStr(name: String, js_value: String)
case class CometLazyDataJObj(path: String, template_js_map: Map[String, _] = Map.empty,
                             jd_data: Map[String, _] = Map.empty, locRes: Option[String] = None)
case class CometLazyDataJVal(path: String, template_js_value: Option[JObject] = None,
                             jd_data: Option[JObject] = None, locRes: Option[String] = None)
case class CometListen(l: CometListener)
case class StopListen()

object CometContext {
  val system: ActorSystem = ActorSystem("CometSystem")
  implicit val executionContext: MessageDispatcher = system.dispatchers.lookup("akka.actor.comet-dispatcher")


}

trait CometListener {
  def cometOut(json: String): Unit
}

trait CometActor extends Actor {
  private var listener: Option[CometListener] = None
  private val stack: mutable.Queue[String] = mutable.Queue.empty
  
  protected def sendObj(st: String): Unit = {
    Logger.debug("sendObj")
    stack enqueue st
    Logger.debug("sendObj next")
    sendAllStacked()
  }
  protected def sendJson(cmd: String, value: String): Unit = {
    Logger.debug("sendJson")
    stack enqueue "{\"%s\": %s}".format(cmd, value)
    Logger.debug("sendJson next")
    sendAllStacked()
  }
  private def sendAllStacked(): Unit = {
    Logger.debug("sendAllStacked listener: " + listener)
    listener.foreach(l => {
      val stacked = "[%s]".format(stack.reduce("%s, %s".format(_, _)))
      Logger.debug("sendAllStacked: " + stacked)
      stack.clear
      l.cometOut(stacked)
    })
    //context.become(onMessage(listener))
  }

  def receive: Receive = onMessage()

  private def onMessage(): Receive = {
    case CometUpdate() => sendJson("update", "1")
    case CometUpdateJObj(name, js) => sendJson(name, Jckson.serialize(js))
    case CometUpdateJVal(name, jv) => sendJson(name, jv match {
      case s: JString => s.textValue()
      case x => x.toString
    })
    case CometUpdateJValStr(name, jv) => sendJson(name, jv)
    case CometLazyDataJObj(path, templjs, data, locRes) => sendJson("lazydata",
      {
        var map: Map[String, Any] = Map("path" -> path)
        if (locRes.nonEmpty) map += "locnames" -> locRes.toList
        if (templjs.nonEmpty) map += "templjs" -> templjs
        if (data.nonEmpty) map += "data" -> data
        Jckson.serialize(map)
      })
    case CometLazyDataJVal(path, templjs, data, locRes) => sendJson("lazydata",
      {
        val fields = List(
          JField("path", JString(path)),
        ) ::: locRes.map(locName => JField("locnames", JArray(List(JString(locName))))).toList :::
          templjs.map(JField("templjs", _)).toList ::: data.map(JField("data", _)).toList

        Jckson.serialize(JObject(fields))
      }
    )
    case CometUpdateEval(script) =>
      sendJson("_eval", "\"%s\"".format(script.replace("\"", "\\\"")))
    case CometListen(l) =>
      Logger.debug("Set comet listener: " + l)
      Logger.debug(this)
      listener = Some(l)
      Logger.debug(this)
      //context.become(onMessage(Some(l)))
      if (stack.nonEmpty) {
        sendAllStacked()
      }
    case cmd => Logger.debug("Unknown comet command: %s %s".format(cmd.toString, this))
  }
}