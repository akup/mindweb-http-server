package com.nn.regbox

import java.io.File
import java.security.SecureRandom
import java.util.{Properties, Random}

import com.nn.mindweb.server.ClosableWrapper
import net.aklabs.Props
import org.pmw.tinylog.writers.{VMShutdownHook, Writer}
import org.pmw.tinylog.{Configurator, Logger}


object BootstrapDI {
  var bootstrapper: Option[Bootstrapper] = None

  Logger.debug("BootstrapDI")
  val tinylogfile = new File("./base/tinylog.properties")
  if (tinylogfile.exists())
    Configurator.fromFile(tinylogfile).addWriter(new PostWriter()).activate()
  else {
    Logger.debug("Activate writer")

    Configurator.fromResource("tinylogfile.properties")
      .activate()

    Logger.debug("Activate writer")
    /*
    Configurator.fromResource("tinylogfile.properties")
      .addWriter(new PostWriter()).activate()
     */
  }

  //чистим список writers для того чтобы наш ShutdownHook 100% залогировался
  private val (shWriters, allWriters) = {
    import collection.JavaConverters._

    val f = classOf[VMShutdownHook].getDeclaredField("writers"); //NoSuchFieldException
    f.setAccessible(true);

    val shwriters =  f.get(null).asInstanceOf[java.util.ArrayList[Writer]]
    val all = Seq() ++ shwriters.asScala
    shwriters.clear()
    Logger.info("shwriters:{} all:{}", shwriters.size().toString, all.size.toString)
    (shwriters, all)
  }

  //закрываем все writers, нужно делать в конце ShutdownHook'а в RegBoxHelpers
  def closeAllWriters(): Unit ={
    allWriters.foreach(w => try{w.close()} catch {
      case e: Throwable => Logger.error(e)//e.printStackTrace()
    })
  }
}

trait Bootstrapper {

  val props: Properties = System.getProperties

  val isDevMode: Boolean = Props.getBoolean("devmode", false)

  def getDeviceId: Option[String] = None
  def getCurrentExpoUID: Option[String] = None
  def getCurrentExpoToken: Option[String] = None
  def getHostStr(secure: Boolean = false) = if (secure) {
    if (isDevMode) new StringBuilder("http://stage.nexpo.me/")
    else new StringBuilder("http://nexpo.me/")
  } else {
    if (isDevMode) new StringBuilder("http://stage.nexpo.me/")
    else new StringBuilder("http://nexpo.me/")
  }
  
  def init(initParam: Option[ClosableWrapper])
  
  private lazy val _slowRandom = new SecureRandom
  protected def withRng[T](block: (Random)=>T): T = {
    _slowRandom.synchronized(block(_slowRandom))
  }
  
  def getRandTicket(): String = {
    (1000000 + withRng(_.nextInt(9000000))).toString
  }
}