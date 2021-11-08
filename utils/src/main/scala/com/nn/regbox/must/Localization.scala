package com.nn.regbox.must

import java.io.StringWriter

import org.apache.commons.io.IOUtils

import scala.collection.JavaConverters._
import scala.collection.MapLike
import scala.collection.immutable.Map
import java.io.File
import java.io.FileInputStream

import net.aklabs.helpers.JsonHelpers.{JObject, Jckson}
import java.io.FileNotFoundException

import com.fasterxml.jackson.databind.JsonNode
import com.nn.http.{R, RoundTripInfo}
import com.nn.regbox.BootstrapDI
import net.aklabs.Props
import org.pmw.tinylog.Logger

object MustacheLoc {
  private val props = System.getProperties
  private var locCache: scala.collection.Map[String, Array[String]] =
      new java.util.concurrent.ConcurrentHashMap[String, Array[String]]().asScala

  private val localizationsPath = "%s/%s".format(
    System.getProperty("com.nn.regbox.docRoot"),
    System.getProperty("com.nn.regbox.localizations"))
  
  def apply(resourceName: String, lng: String, rootPath: Option[String] = None): Map[String, _] = {
    val path = "%s%s_%s.mustloc".format(rootPath.getOrElse(localizationsPath),
      resourceName, lng)
    var keyLine: String = ""
    var lastLines: String = ""
    //Logger.debug("MustacheLoc: " + BootstrapDI.bootstrapper.exists(_.isDevMode))
    val lines: Array[String] = try {if (!Props.getBoolean("devmode", false)) {
      Logger.debug("Localization cached")
      locCache.getOrElse(path, {
        val writer = new StringWriter()
        val file = new File(path)
        IOUtils.copy(new FileInputStream(file), writer, "UTF-8")
        val res = writer.toString.split("\n")
        locCache += path -> res
        res
      })
    }
    else {
      Logger.debug("Localization not cached")
      val writer = new StringWriter()
      val file = new File(path)
      IOUtils.copy(new FileInputStream(file), writer, "UTF-8")
      writer.toString.split("\n")
    }} catch {
      case _: FileNotFoundException => Array()
    }

    lines.flatMap(line => {
      if (keyLine.isEmpty && line.trim.endsWith(":")) {
        keyLine = line.trim.dropRight(1)
        None
      }
      else if (line == "\"\"\"") {
        val res = (keyLine, lastLines)
        lastLines = ""
        keyLine = ""
        Some(res)
      } else {
        lastLines += line
        None
      }
    })
  }.toMap
}

case class RoundTripData(name: String, info: Seq[RoundTripInfo])
object LocViewFactory {
  def default_language: String = R.httpSession.flatMap(
    _.attribute("locale").map(_.toString)
  ).getOrElse("ru")
  
  def apply(tmplFile: Option[String], tmpl: Option[String], json: Option[String] = None): View = {
    json match {
      case Some(js) => new View {
        override def templateString: Option[String] = tmpl
        override def templateFileName: Option[String] = tmplFile
        
        val json: Map[String, _] = Jckson.deserialize[Map[String, _]](js)
      }
      case _ => new View {
        override def templateString: Option[String] = tmpl
        override def templateFileName: Option[String] = tmplFile
      }
    }
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], js: Map[String, _]): View = {
    new View {
      override def templateString: Option[String] = tmpl
      override def templateFileName: Option[String] = tmplFile
      
      val json: Map[String, _] = js
    }
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String], json: Option[String], roundTrip: Option[RoundTripData]): View = {
    val lng = l.getOrElse(default_language)
    json match {
      case Some(js) => new View {
        override def templateString: Option[String] = tmpl
        override def templateFileName: Option[String] = tmplFile
        
        val json = localizeJson(Jckson.deserialize[Map[String, _]](js), lng)

        val mw_page = R.renderVersion
        val load_default_scripts: String = R.defaultScripts
        val roundtrip_script: String = roundTrip.map(rt => {
          """var %s = %s;""".stripMargin.format(rt.name, R.session.get.buildRoundtrip(rt.info))
        }).getOrElse{R.session.get.buildRoundtrip(Nil); ""}

        val loc: Map[String, _] = MustacheLoc(loc_res, lng)
        val locale: Map[String, Boolean] = Map(lng -> true)
        val locale_script: String = "<script>window.locale = '" + lng + "';</script>"
      }
      case _ => new View {
        override def templateString: Option[String] = tmpl
        override def templateFileName: Option[String] = tmplFile

        val mw_page = R.renderVersion
        val load_default_scripts: String = R.defaultScripts
        val enable_ajax: String = """<script>window.addEventListener('DOMContentLoaded', function() {%s})</script>""".format(R.ajaxScript())
        val roundtrip_script: String = roundTrip.map(rt => {
          """var %s = %s;""".stripMargin.format(rt.name, R.session.get.buildRoundtrip(rt.info))
        }).getOrElse{R.session.get.buildRoundtrip(Nil); ""}

        val loc: Map[String, _] = MustacheLoc(loc_res, lng)
        val locale: Map[String, Boolean] = Map(lng -> true)
        val locale_script: String = "<script>window.locale = '" + lng + "';</script>"
      }
    }
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String]): View = {
    apply_jsmap(tmplFile, tmpl, loc_res, l, Map.empty, None)
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String], roundTrip: Option[RoundTripData]): View = {
    apply_jsmap(tmplFile, tmpl, loc_res, l, Map.empty, roundTrip)
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String], js: Map[String, _]): View = {
    apply_jsmap(tmplFile, tmpl, loc_res, l, js, None)
  }
  def apply(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String], js: Map[String, _], roundTrip: Option[RoundTripData]): View = {
    apply_jsmap(tmplFile, tmpl, loc_res, l, js, roundTrip)
  }
  
  private def apply_jsmap(tmplFile: Option[String], tmpl: Option[String], loc_res: String, l: Option[String], js: Map[String, _], roundTrip: Option[RoundTripData]): View = {
    var lng = l.getOrElse(default_language)
    if (lng.length() == 0){
      lng = default_language
    }
    new View {
      override def templateString: Option[String] = tmpl
      override def templateFileName: Option[String] = tmplFile
      val json: Any = localizeJson(js, lng)

      val mw_page: String = R.renderVersion
      val load_default_scripts: String = R.defaultScripts
      val enable_ajax: String = """<script>window.addEventListener('DOMContentLoaded', function() {%s})</script>""".format(R.ajaxScript())
      val roundtrip_script: String = roundTrip.map(rt => {
        """var %s = %s;""".stripMargin.format(rt.name, R.session.get.buildRoundtrip(rt.info))
      }).getOrElse{R.session.get.buildRoundtrip(Nil); ""}

      val loc: Map[String, _] = MustacheLoc(loc_res, lng)
      val locale: Map[String, Any] = Map(lng -> true, "lng" -> lng)
      val locale_script: String = "<script>window.locale = '" + lng + "';</script>"
      
      //Logger.info("json:" + json)
      //Logger.info("loc:" + loc)
      //Logger.info("locale:" + locale)
      //Logger.info("locale_script:" + locale_script)
    }
  }
    
  def localizeJson(js: Any, l: String): Any = {
    val res = js match {
      case json: JObject =>
        val js = Jckson.deserialize[Map[String, _]](json.toString)
        js.map(kv => {
          (if (kv._1 == l) "lc" else kv._1) -> localizeJson(kv._2, l)
        })
      case js: MapLike[_, _, _] =>
        js.map(kv => {
          (if (kv._1 == l) "lc" else kv._1) -> localizeJson(kv._2, l)
        })
      case x: Iterable[_] => x.map(localizeJson(_, l))
      case x => x
    }
    res
  }
}