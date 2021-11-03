package net.aklabs

import java.net.URL
import java.util.Properties

import net.aklabs.helpers.Helpers._
import org.pmw.tinylog.Logger

import scala.collection.mutable.ListBuffer

/*
* A trait for identfying Neo4J instances
*/
object Props {
  var mode: Modes.Value = if (System.getProperty("production") == "true") Modes.prod else Modes.debug

  def productionMode: Boolean = mode == Modes.prod

  lazy val props: Properties = {
    val prop = new Properties()
    val is = getClass.getClassLoader.getResourceAsStream("props/%s.props".format(mode.toString))
    if (is != null) try {
      prop.load(is)
    } finally {
      is.close()
    }
    prop
  }

  def initVolumes() = {
    System.setProperty("service.files", get("service.files").getOrElse("./"))
  }

  def get(key: String): Option[String] = Option(props.getProperty(key))
  def get(key: String, default: String): String = props.getProperty(key, default)
  def getInt(key: String): Option[Int] = Option(props.getProperty(key)).flatMap(i => tryo{i.toInt})
  def getInt(key: String, default: Int): Int =
    Option(props.getProperty(key)).flatMap(i => tryo{i.toInt}) getOrElse default
  def getLong(key: String): Option[Long] = Option(props.getProperty(key)).flatMap(i => tryo{i.toLong})
  def getLong(key: String, default: Long): Long =
    Option(props.getProperty(key)).flatMap(i => tryo{i.toLong}) getOrElse default
  def getBoolean(key: String): Option[Boolean] = Option(props.getProperty(key)).flatMap(i => tryo{i.toBoolean})
  def getBoolean(key: String, default: Boolean): Boolean =
    Option(props.getProperty(key)).flatMap(i => tryo{i.toBoolean}) getOrElse default

  object Modes extends Enumeration {
    type Modes = Value
    val debug, prod = Value
  }



  def getResourceUrl(path: String): Option[URL] = Option(getClass.getResource(path))

  var unloadHooks: ListBuffer[() => Unit] = ListBuffer[() => Unit]()

  Runtime.getRuntime.addShutdownHook(new Thread(
    () => {
      unloadHooks.foreach(f => try {
        f()
      } catch {case e: Throwable => Logger.error(e)/*e.printStackTrace()*/})
    }))
}