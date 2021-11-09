/**
* Copyright (C) 2012 Twitter Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package net.aklabs.regbox.must

import com.github.mustachejava.{DefaultMustacheFactory, Mustache}
import net.aklabs.Props

import java.io._
import java.util.concurrent.{Callable, ConcurrentHashMap, Executors}
//import com.github.mustachejava.MustacheFactory
import com.github.mustachejava.MustacheException
import com.twitter.mustache.ScalaObjectHandler
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.collection.concurrent
//import org.pmw.tinylog.Logger

trait MindwebMustacheFactoryTrait extends DefaultMustacheFactory {
	/**
	 * Invalidate template caches during development
	 */
	def invalidateMustacheCaches(): Unit
}
class MindwebMustacheFactory(baseTemplatePath:String) extends MindwebMustacheFactoryTrait {
	private def combinePaths(path1: String, path2: String): String = {
		new File(new File(path1), path2).getPath
  }

	override def getReader(resourceName:String): Reader = {
		val fileName = if (resourceName contains ".mustache") resourceName else resourceName+".mustache"
		val basePath = baseTemplatePath//combinePaths(System.getProperty("com.nn.regbox.absRoot"), baseTemplatePath)
		val file:File = new File(basePath, fileName)
		if (file.exists() && file.isFile) {
			try {
				new BufferedReader(new InputStreamReader(new FileInputStream(file),"UTF-8"))
			} catch {
				case exception:FileNotFoundException =>
					throw new MustacheException("Found Mustache file, could not open: " + file + " at path: " + basePath, exception)
			}
		} else {
			throw new MustacheException("Mustache Template '" + resourceName + "' not found at " + file + " at path: " + basePath)
		}
	}

	/**
	 * Invalidate template caches during development
	 */
	def invalidateMustacheCaches(): Unit = {
		mustacheCache.clear()
		templateCache.clear()
	}
}

object View {
	private[must] val mustacheCache: concurrent.Map[String, Any] = new ConcurrentHashMap[String, Any]().asScala

	var baseTemplatePath: String = System.getProperty("com.nn.regbox.docRoot")
	def templatePath: String = baseTemplatePath

	lazy val mustacheFactory = new MindwebMustacheFactory(baseTemplatePath)
	mustacheFactory.setObjectHandler(new ScalaObjectHandler)
	mustacheFactory.setExecutorService(Executors.newCachedThreadPool)
	private def combinePaths(path1: String, path2: String): String = {
		//Logger.info("combining "+path1+" with "+path2)		
		new File(path1, path2).getPath
	}
}

abstract class View extends Callable[String] {
	def templateString: Option[String] = None
	def templateFileName: Option[String] = None
	new DefaultMustacheFactory()
	def templatePath: Option[String] = templateFileName.map(template => View.combinePaths(baseTemplatePath, template))
	val factory: MindwebMustacheFactoryTrait = View.mustacheFactory
	var baseTemplatePath: String = View.templatePath
	var contentType: Option[String] = None

	var cacheName: Option[String] = None

	protected lazy val sm = "{{"
	protected lazy val em = "}}"

	private val file: Option[File] = templatePath.flatMap{p =>
		val f = new File(p)
		if (p.isEmpty) None else Some(f)
	}
	private val mustacheStr: Option[String] = templateString
	def mustache: Mustache = {
	  val t1 = System.nanoTime()
		file.map(file => {
			//TODO: mustache cache
			val m = factory.compile(
				new InputStreamReader(new FileInputStream(file), "UTF-8"), "template",
				sm, em)
			val t2 = System.nanoTime()
			Logger.debug("Mustache compile time: " + (t2 - t1))
			m
		}).getOrElse{
			val m = factory.compile(new StringReader(mustacheStr.get), "template",
				sm, em)
			val t2 = System.nanoTime()
			Logger.debug("Mustache compile time: " + (t2 - t1))
			m
		}
	}
	def render: String = {
	  //Logger.info("TRY RENDER")
	  val t1 = System.nanoTime()
		if(Props.getBoolean("devmode", default = false)){
			factory.invalidateMustacheCaches()
		}	
	  val t2 = System.nanoTime()
	  //Logger.info("AT RENDER: " + (t2 - t1))
		val output = new StringWriter
		//Logger.info("BEFORE EXECUTE")
		mustache.execute(output, this).flush()
		//Logger.info("DONE EXECUTE")
		output.toString
	}

	def call: String = render
}