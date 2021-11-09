/*
 * Copyright 2006-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.aklabs.http

import java.util.ResourceBundle
import net.aklabs.Props
import net.aklabs.helpers._
import vars.{RequestVarHandler, TransientRequestVar, TransientRequestVarHandler}
import org.pmw.tinylog.Logger

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/**
 * An object representing the current state of the HTTP request and response.
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around. The S object
 * is initialized by LiftSession on request startup.
 *
 * @see LiftSession
 * @see LiftFilter
 */
object R extends R {

  private[http] class ProxyFuncHolder(proxyTo: AFuncHolder, _owner: Option[String]) extends AFuncHolder {
    def this(proxyTo: AFuncHolder) = this (proxyTo, None)

    def owner: Option[String] = _owner orElse proxyTo.owner

    def apply(in: List[String]): Any = proxyTo.apply(in)

    override def apply(in: FileParamHolder): Any = proxyTo.apply(in)

    override def supportsFileParams_? : Boolean = proxyTo.supportsFileParams_?

    override private[http] def lastSeen: Long = proxyTo.lastSeen

    override private[http] def lastSeen_=(when: Long): Unit = proxyTo.lastSeen = when

    override def sessionLife: Boolean = proxyTo.sessionLife
  }

  /**
   *  Impersonates a function that will be called when uploading files
   */
  private final class BinFuncHolder(val func: FileParamHolder => Any, val owner: Option[String]) extends AFuncHolder with Serializable{
    def apply(in: List[String]): Unit = {Logger.info("You attempted to call a 'File Upload' function with a normal parameter.  Did you forget to 'enctype' to 'multipart/form-data'?")}

    override def apply(in: FileParamHolder): Any = func(in)

    override def supportsFileParams_? : Boolean = true
  }

  object BinFuncHolder {
    def apply(func: FileParamHolder => Any): AFuncHolder = new BinFuncHolder(func, None)

    def apply(func: FileParamHolder => Any, owner: Option[String]): AFuncHolder = new BinFuncHolder(func, owner)
  }

  object SFuncHolder {
    def apply(func: String => Any): AFuncHolder = new SFuncHolder(func, None)

    def apply(func: String => Any, owner: Option[String]): AFuncHolder = new SFuncHolder(func, owner)
  }

  /**
   * Impersonates a function that is executed on HTTP requests from client. The function
   * takes a String as the only parameter and returns an Any.
   */
  private final class SFuncHolder(val func: String => Any, val owner: Option[String]) extends AFuncHolder with Serializable{
    def apply(in: List[String]): Any = in.headOption.toList.map(func(_))
  }

  object LFuncHolder {
    def apply(func: List[String] => Any): AFuncHolder = new LFuncHolder(func, None)

    def apply(func: List[String] => Any, owner: Option[String]): AFuncHolder = new LFuncHolder(func, owner)
  }

  /**
   * Impersonates a function that is executed on HTTP requests from client. The function
   * takes a List[String] as the only parameter and returns an Any.
   */
  private final class LFuncHolder(val func: List[String] => Any, val owner: Option[String]) extends AFuncHolder with Serializable {
    def apply(in: List[String]): Any = func(in)
  }

  object NFuncHolder {
    def apply(func: () => Any): AFuncHolder = new NFuncHolder(func, None)

    def apply(func: () => Any, owner: Option[String]): AFuncHolder = new NFuncHolder(func, owner)
  }

  /**
   * Impersonates a function that is executed on HTTP requests from client. The function
   * takes zero arguments and returns an Any.
   */
  private final class NFuncHolder(val func: () => Any, val owner: Option[String]) extends AFuncHolder with Serializable{
    def apply(in: List[String]): Any = in.headOption.toList.map(_ => func())
  }

  /**
   * Abstrats a function that is executed on HTTP requests from client.
   */
  sealed trait AFuncHolder extends (List[String] => Any) with Serializable{
    def owner: Option[String]

    def apply(in: List[String]): Any

    def apply(in: FileParamHolder): Any = {
      sys.error("Attempt to apply file upload to a non-file upload handler")
    }

    def supportsFileParams_? : Boolean = false

    def duplicate(newOwner: String): AFuncHolder = new ProxyFuncHolder(this, Some(newOwner))

    @volatile private[this] var _lastSeen: Long = System.currentTimeMillis()

    private[http] def lastSeen: Long = _lastSeen

    private[http] def lastSeen_=(when: Long): Unit = _lastSeen = when

    def sessionLife: Boolean = _sessionLife

    private[this] val _sessionLife: Boolean = functionLifespan_?
  }

  /**
   * We create one of these dudes and put it
   */
  private[http] final case class PageStateHolder(owner: Option[String], session: TheSession) extends AFuncHolder {
    private val snapshot:  (() => Any) => Any = RequestVarHandler.generateSnapshotRestorer()
    override def sessionLife: Boolean = false

    def apply(in: List[String]): Any = {
      sys.error("You shouldn't really be calling apply on this dude...")
    }

    def runInContext[T](f: => T): T = {
      val ret = snapshot(() => f).asInstanceOf[T]
      ret
    }
  }

  /**
   * The companion object that generates AFuncHolders from other functions
   */
  object AFuncHolder {
    implicit def strToAnyAF(f: String => Any): AFuncHolder =
      SFuncHolder(f)

    implicit def unitToAF(f: () => Any): AFuncHolder = NFuncHolder(f)

    implicit def listStrToAF(f: List[String] => Any): AFuncHolder =
      LFuncHolder(f)

    implicit def boolToAF(f: Boolean => Any): AFuncHolder =
      LFuncHolder(lst => f(lst.foldLeft(false)(
        (v, str) => v || Helpers.toBoolean(str))))
  }

}


/**
 * An object representing the current state of the HTTP request and response.
 * It uses the DynamicVariable construct such that each thread has its own
 * local session info without passing a huge state construct around. The S object
 * is initialized by LiftSession on request startup.
 *
 * @see LiftSession
 * @see LiftFilter
 */
trait R {
  import R._

  /*
   * The current session state is contained in the following val/vars:
   */

  /**
   * Holds the current Req (request) on a per-thread basis.
   * @see Req
   */
  private val _request = new RichThreadLocal[HttpRequest]

  /**
   * Holds the current functions mappings for this session.
   *
   * @see # functionMap
   * @see # addFunctionMap
   * @see # clearFunctionMap
   */
  private val __functionMap = new RichThreadLocal[Map[String, AFuncHolder]]

  /**
   * This is simply a flag so that we know whether or not the state for the S object
   * has been initialized for our current scope.
   *
   * @see # inStatefulScope_ ?
   * @see # initIfUninitted
   */
  private val inS = new RichThreadLocal[Boolean].set(false)

  /**
   * Holds the per-request LiftSession instance.
   *
   * @see LiftSession
   * @see # session
   */
  private val _sessionInfo = new RichThreadLocal[TheSession]

  /**
   * Holds a list of ResourceBundles for this request.
   *
   * @see # resourceBundles
   * @see LiftRules # resourceNames
   * @see LiftRules # resourceBundleFactories
   */
  private val _resBundle = new RichThreadLocal[List[ResourceBundle]]
  private val _lifeTime = new RichThreadLocal[Boolean]
  private val autoCleanUp = new RichThreadLocal[Boolean]

  private object _exceptionThrown extends TransientRequestVar(false)

  private object postFuncs extends TransientRequestVar(new ListBuffer[() => Unit])

  private object p_queryLog extends TransientRequestVar(new ListBuffer[(String, Long)])

  /**
   * This method returns true if the S object has been initialized for our current scope. If
   * the S object has not been initialized then functionality on S will not work.
   */
  def inStatefulScope_? : Boolean = inS.value

  /**
   * Get a Req representing our current HTTP request.
   *
   * @return A Full(Req) if one has been initialized on the calling thread, Empty otherwise.
   *
   * @see Req
   */
  def request: Option[HttpRequest] = _request.option orElse CurrentReq.option


  /**
   * An exception was thrown during the processing of this request.
   * This is tested to see if the transaction should be rolled back
   */
  def assertExceptionThrown(): Unit = {_exceptionThrown.set(true)}

  /**
   * Was an exception thrown during the processing of the current request?
   */
  def exceptionThrown_? : Boolean = _exceptionThrown.get

  def renderVersion: String = RenderVersion.get
  
  
  def build_num: String = if (Props.mode == Props.Modes.prod) "0" else Helpers.nextFuncName
    
  def defaultCss: String = {
    val bn = build_num
    val break = if (Props.mode == Props.Modes.debug) "\n" else ""
    Rules.defaultCss.map(css => {
      """<link rel="stylesheet" type="text/css" media="%s" href="%s%s?v=%s"/>%s"""
      	.format(css._1, Rules.defaultJsScriptLocation, css._2, bn, break)
    }).mkString
  }
  def defaultScripts: String = {
    val bn = build_num
    val break = if (Props.mode == Props.Modes.debug) "\n" else ""
    Rules.defaultJsScripts.map(scr => {
      """<script %s src="%s%s?v=%s" type="text/javascript"></script>%s"""
      	.format(scr._2, Rules.defaultJsScriptLocation, scr._1, bn, break)
    }).mkString
  }
  def angularPlatformScripts: String = {
    val bn = build_num
    val break = if (Props.mode == Props.Modes.debug) "\n" else ""
    Rules.defaultAngularJsScripts.map(scr => {
      """<script %s src="%s%s?v=%s" type="text/javascript"></script>%s"""
        .format(scr._2, Rules.defaultAngularJsScriptLocation, scr._1, bn, break)
    }).mkString
  }
  def ajaxScript(extra: Option[String] = None): String = {
    """jQuery(document).ready(function() {mwAjax.scheduleBeat();});
    |window.mw_page = "%s"; %s
    |mwAjax.cometSuccess();
    |%s""".stripMargin.format(renderVersion,
        session.map(s => "window.mw_session = \"%s\";".format(s.uniqueId)).getOrElse(""), extra.getOrElse(""))
  }

  /**
   * Initialize the current request session. Generally this is handled by Lift during request
   * processing, but this method is available in case you want to use S outside the scope
   * of a request (standard HTTP or Comet).
   *
   * @param request The Req instance for this request
   * @param session the LiftSession for this request
   * @param f Function to execute within the scope of the request and session
   */
  def init[B](request: HttpRequest, session: TheSession)(f: => B): B = {
    if (inS.value) f
    else {
      if (request.stateless_?)
        session.doAsStateless(_init(request, session)(() => f))
      else _init(request, session)(() => f)
    }
  }

  def statelessInit[B](request: HttpRequest)(f: => B): B = {
    session match {
      case Some(s) =>
        if (s.stateful_?)
	        throw new IllegalAccessException(
	          "Attempt to initialize a stateless session within the context "+
	          "of a stateful session")
        f

      case _ =>
        val fakeSess = new TheSession(Helpers.nextFuncName, None) with StatelessSession
        try {
          _init(request,
                fakeSess)(() => f)
        } finally {
          fakeSess.doShutDown()
        }
    }
  }

  /**
   * The current LiftSession.
   */
  def session: Option[TheSession] = Option(_sessionInfo.value)
  
  def makeRequestVarsSnapshot(): Unit = session.foreach(_.makeRequestVarsSnapshot())

  /**
   * Log a query for the given request.  The query log can be tested to see
   * if queries for the particular page rendering took too long. The query log
   * starts empty for each new request. net.liftweb.mapper.DB.queryCollector is a
   * method that can be used as a log function for the net.liftweb.mapper.DB.addLogFunc
   * method to enable logging of Mapper queries. You would set it up in your bootstrap like:
   *
   * <pre name="code" class="scala" >
   * import net.liftweb.mapper.DB
   * import net.liftweb.http.S
   * class Boot  {
   *   def boot  {
   *     ...
   *     DB.addLogFunc(DB.queryCollector)
   *     ...
   * }
   * }
   * </pre>
   *
   * Note that the query log is simply stored as a List and is not sent to any output
   * by default. To retrieve the List of query log items, use S.queryLog. You can also
   * provide your own analysis function that will process the query log via S.addAnalyzer.
   *
   * @see # queryLog
   * @see # addAnalyzer
   * @see net.liftweb.mapper.DB.addLogFunc
   */
  def logQuery(query: String, time: Long): ListBuffer[(String, Long)] = p_queryLog.is += ((query, time))

  /**
   * Returns the current value of the given HTTP request header as a Box. This is
   * really just a thin wrapper on Req.header(String). For response headers, see
   * S.getHeaders, S.setHeader, or S.getHeader.
   *
   * @param name The name of the HTTP header to retrieve
   * @return A Full(value) or Empty if the header isn't set
   *
   * @see Req # header ( String )
   * @see # getHeader ( String )
   * @see # setHeader ( String, String )
   * @see # getHeaders ( List[ ( String, String ) ] )
   */
  def getRequestHeader(name: String): Option[String] =
    for (req <- request;
         hdr <- req.getHeader(name))
    yield hdr

  /**
   * Adds a cleanup function that will be executed at the end of the request pocessing.
   * Exceptions thrown from these functions will be swallowed, so make sure to handle any
   * expected exceptions within your function.
   *
   * @param f The function to execute at the end of the request.
   */
  def addCleanupFunc(f: () => Unit): Unit = postFuncs.is += f

  /**
   * Are we currently in the scope of a stateful request
   */
  def statefulRequest_? : Boolean = session match {
    case Some(s) => s.stateful_?
    case _ => false
  }

  private def _innerInit[B](request: HttpRequest, f: () => B): B = {
    _lifeTime.doWith(false) {
      _resBundle.doWith(Nil) {
        inS.doWith(true) {
          withReq(request) {
            __functionMap.doWith(Map()) {f()}
          }
        }
      }
    }
  }

  private[http] def withReq[T](req: HttpRequest)(f: => T): T = {
    CurrentReq.doWith(req) {
      _request.doWith(req) {
        f
      }
    }
  }

  private def _init[B](request: HttpRequest, session: TheSession)(f: () => B): B =
    this._request.doWith(request) {
      _sessionInfo.doWith(session) {
        TransientRequestVarHandler(Some(session),
          RequestVarHandler(Some(session), {
            val res = _innerInit(request, f)
            session.makeRequestVarsSnapshot()
            res
          })
        )
      }
    }

  /**
   * This method is a convenience accessor for LiftRules.loggedInTest. You can define your own
   * function to check to see if a user is logged in there and this will call it.
   *
   * @see LiftRules.loggedInTest
   *
   * @return the value from executing LiftRules.loggedInTest, or <code>false</code> if a test function
   * is not defined.
   */
  def loggedIn_? : Boolean = Rules.loggedInTest.map(_.apply()) openOr false

  /**
   * Returns the 'Referer' HTTP header attribute.
   */
  def referer: Option[String] = request.flatMap(_.getHeader("Referer"))


  /**
   * Functions that are mapped to HTML elements are, by default,
   * garbage collected if they are not seen in the browser in the last 10 minutes (defined in LiftRules.unusedFunctionsLifeTime).
   * In some cases (e.g., JSON handlers), you may want to extend the
   * lifespan of the functions to the lifespan of the session.
   *
   * @param span If <code>true</code>, extend the mapped function lifetime to the life of the session
   * @param f A function to execute in the context of specified span
   *
   * @see LiftRules.unusedFunctionsLifeTime
   */
  def functionLifespan[T](span: Boolean)(f: => T): T =
    _lifeTime.doWith(span)(f)

  /**
   * Returns whether functions are currently extended to the lifetime of the session.
   *
   * @return <code>true</code> if mapped functions will currently last the life of the session.
   */
  def functionLifespan_? : Boolean = _lifeTime.option getOrElse false

  /**
   * Initialize the current request session if it's not already initialized.
   * Generally this is handled by Lift during request processing, but this
   * method is available in case you want to use S outside the scope
   * of a request (standard HTTP or Comet).
   *
   * @param session the LiftSession for this request
   * @param f A function to execute within the scope of the session
   */
  def initIfUninitted[B](session: TheSession)(f: => B): B = {
    if (inS.value) f
    else init(HttpRequest.nil(session.httpSession), session)(f)
  }

  /**
   * Returns the LiftSession parameter denominated by 'what'.
   *
   * @see # getSessionAttribute
   * @see # set
   * @see # setSessionAttribute
   * @see # unset
   * @see # unsetSessionAttribute
   */
  def get(what: String): Option[String] = session.flatMap(_.get[String](what))

  /**
   * Returns the HttpSession parameter denominated by 'what'
   *
   * @see # get
   * @see # set
   * @see # setSessionAttribute
   * @see # unset
   * @see # unsetSessionAttribute
   *
   */
  def getSessionAttribute(what: String): Option[String] = httpSession.flatMap(_.attribute(what) match {case Some(s: String) => Some(s) case _ => None})

  /**
   * Returns the HttpSession
   */
  def httpSession: Option[HttpSession] = session.flatMap(_.httpSession).orElse(request.flatMap(_.getSession))

  /**
   * Returns the 'type' S attribute. This corresponds to the current Snippet's name. For example, the snippet:
   *
   * <pre name="code" class="xml">
  &lt;lift:Hello.world /&gt;
   * </pre>
   *
   * Will return "Hello.world".
   */
  //def invokedAs: String = (currentSnippet or attr("type")) openOr ""

  /**
   * Sets a HttpSession attribute
   *
   * @see # get
   * @see # getSessionAttribute
   * @see # set
   * @see # unset
   * @see # unsetSessionAttribute
   *
   */
  def setSessionAttribute(name: String, value: String): Unit = httpSession.foreach(_.setAttributeGlobal(name, value))
  def setSessionAttributeLocal(name: String, value: String): Unit = httpSession.foreach(_.setAttribute(name, value))

  /**
   * Sets a LiftSession attribute
   *
   * @see # get
   * @see # getSessionAttribute
   * @see # setSessionAttribute
   * @see # unset
   * @see # unsetSessionAttribute
   *
   */
  def set(name: String, value: String): Unit = session.foreach(_.set(name, value))

  /**
   * Removes a HttpSession attribute
   *
   * @see # get
   * @see # getSessionAttribute
   * @see # set
   * @see # setSessionAttribute
   * @see # unset
   *
   */
  def unsetSessionAttribute(name: String): Unit = httpSession.foreach(_.removeAttribute(name))

  /**
   * Removes a LiftSession attribute
   *
   * @see # get
   * @see # getSessionAttribute
   * @see # set
   * @see # setSessionAttribute
   * @see # unsetSessionAttribute
   *
   */
  def unset(name: String): Unit = session.foreach(_.unset(name))

  /**
   * The hostname to which the request was sent. This is taken from the "Host" HTTP header, or if that
   * does not exist, the DNS name or IP address of the server.
   */
  def hostName: String = request.map(_.remoteHost) getOrElse "127.0.0.1"// Req.localHostName

  /**
   * The host and path of the request up to and including the context path. This does
   * not include the template path or query string.
   */
  def hostAndPath: String = request.map(r => "%s/%s".format(r.remoteHost, r.path)) getOrElse ""

  /**
   * Get a map of function name bindings that are used for form and other processing. Using these
   * bindings is considered advanced functionality.
   */
  def functionMap: Map[String, AFuncHolder] = __functionMap.option.getOrElse(Map())

  private def testFunctionMap[T](f: => T): T =
    session match {
      case Some(s) if s.stateful_? => f
      case _ => throw new IllegalAccessException(
        "Accessing function map information outside of a stateful session")
    }

  /**
   * Clears the function map.  potentially very destuctive... use at your own risk!
   */
  def clearFunctionMap(): Unit = {
    if (__functionMap.option.map(_.size).getOrElse(0) > 0) {
      testFunctionMap {
        __functionMap.option.foreach(_ => __functionMap.set(Map()))
      }
    }
  }

  private def updateFunctionMap(name: String, value: AFuncHolder): Unit = {
    __functionMap.option match {
      case Some(old) =>
        Logger.debug("update __functionMap")
        __functionMap.set(old + ((name, value)))
      case _ =>
    }
  }

  import com.fasterxml.jackson.databind.node._
  /**
   * Associates a name with a function impersonated by AFuncHolder. These are basically functions
   * that are executed when a request contains the 'name' request parameter.
   */
  def addFunctionMap(name: String, value: AFuncHolder): Unit = {
    testFunctionMap {
      if (autoCleanUp.value) {
        updateFunctionMap(name,
          new R.ProxyFuncHolder(value) {
            var shot = false

            override def apply(in: List[String]): Any = {
              synchronized {
                if (!shot) {
                  shot = true
                  R.session.foreach(_.removeFunction(name))
                  value.apply(in)
                } else {
                  NullNode.instance
                  //js.JsCmds.Noop
                }
              }
            }

            override def apply(in: FileParamHolder): Any = {
              synchronized {
                if (!shot) {
                  shot = true
                  R.session.foreach(_.removeFunction(name))
                  value.apply(in)
                } else {
                  NullNode.instance
                  //js.JsCmds.Noop
                }
              }
            }
          })
      } else {
        updateFunctionMap(name, value)
      }
    }
  }

  /**
   * Execute code synchronized to the current session object
   */
  def synchronizeForSession[T](f: => T): T = {
    session match {
      case Some(s) => s.synchronized(f)
      case _ => f
    }
  }

  /**
   * Maps a function with an random generated and name
   */
  def fmapFunc[T](in: AFuncHolder)(f: String => T): T = {
    val name = Helpers.nextFuncName
    Logger.debug("fmapFunc")
    addFunctionMap(name, in)
    f(name)
  }

  /**
   * If you bind functions (i.e. using SHtml helpers) inside the closure passed to callOnce,
   * after your function is invoked, it will be automatically removed from functions cache so
   * that it cannot be invoked again.
   */
  def callOnce[T](f: => T): T = {
    autoCleanUp.doWith(true) {
      f
    }
  }

}
