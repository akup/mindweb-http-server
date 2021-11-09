/*
 * Copyright 2007-2012 WorldWide Conferencing, LLC
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

import akka.actor.{ActorRef, PoisonPill}
import com.fasterxml.jackson.databind._
import net.aklabs.helpers.Helpers._
import net.aklabs.helpers.JsonHelpers.Jckson
import net.aklabs.helpers._
import net.aklabs.http.vars.{RequestVar, RequestVarHandler, SessionVar}
import org.pmw.tinylog.Logger

import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import scala.concurrent.Future

object TheSession {

  val unusedFunctionsLifeTime: Int = 30 * 60 * 1000 // 10 minutes
  /**
   * Returns a reference to a TheSession dictated by LiftRules#sessionCreator function.
   */
  def apply(session: HttpSession) = new TheSession(session.sessionId, Some(session))

  def apply(request: HttpRequest): TheSession =
    if (request.stateless_?) new TheSession(nextFuncName, Empty) with StatelessSession
    else this.apply(request.session.get)

  /**
   * Holds user's functions that will be called when the session is activated
   */
  var onSessionActivate: List[TheSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is passivated
   */
  var onSessionPassivate: List[TheSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is setup
   */
  var onSetupSession: List[TheSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is about to be terminated
   */
  var onAboutToShutdownSession: List[TheSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when the session is terminated
   */
  var onShutdownSession: List[TheSession => Unit] = Nil

  /**
   * Holds user's functions that will be called when a stateful request is about to be processed
   */
  var onBeginServicing: List[(TheSession, HttpRequest) => Unit] = Nil

  /**
   * After the session is created, if you need to do anything within
   * the context of the session (like set SessionVars, etc),
   * add the function to this list
   */
  var afterSessionCreate: List[(TheSession, HttpRequest) => Unit] = Nil

  
  /*
  /**
   * Holds user's functions that will be called when a stateful request has been processed
   */
  var onEndServicing: List[(TheSession, HttpRequest, Box[LiftResponse]) => Unit] = Nil
   */

}


private[http] case class AddSession(session: TheSession)
private[http] case class RemoveSession(sessionId: String)

case class SessionWatcherInfo(sessions: Map[String, SessionInfo])

/**
 * Information about sessions
 */
case class SessionInfo(session: TheSession, userAgent: Option[String], ipAddress: Option[String], requestCnt: Int, lastAccess: Long)




object PageName extends RequestVar[String]("")

/**
 * Information about the page garbage collection
 */
object RenderVersion {

  private object ver extends RequestVar(nextFuncName)

  def get: String = ver.is

  def doWith[T](v: String)(f: => T): T = {
    val ret: Box[T] =
      for {
        sess <- R.session
        func <- sess.findFunc(v).collect {
          case f: R.PageStateHolder => f
        }
      } yield {
        val tret = ver.doWith(v) {
          val ret = func.runInContext(f)

          //Logger.debug("RenderVersion run in context: " + R.functionMap)
          if (R.functionMap.nonEmpty) {
            Logger.debug("UPDATE SESSION FUNCMAP")
            sess.updateFunctionMap(R.functionMap, this.get, System.currentTimeMillis())
            R.clearFunctionMap()
          }
          ret
        }
        tret
      }

    ret openOr ver.doWith(v) {
      val ret = f
      //Logger.debug("RenderVersion run with got functions: " + R.functionMap)
      if (R.functionMap.nonEmpty) {
        R.session.foreach(sess => {
          //Logger.debug("UPDATE SESSION FUNCMAP")
          sess.updateFunctionMap(R.functionMap, this.get, System.currentTimeMillis())
          R.clearFunctionMap()
        })
      }
      ret
    }
  }
}

/**
 * A trait defining how stateful the session is
 */
trait HowStateful {
  private val howStateful = new RichThreadLocal[Boolean]

  /**
   * Test the statefulness of this session.
   */
  def stateful_? : Boolean = Option(howStateful.value) getOrElse true

  /**
   * There may be cases when you are allowed container state (e.g.,
   * migratory session, but you're not allowed to write Lift
   * non-migratory state, return true here.
   */
  //def allowContainerState_? = howStateful.box openOr true

  /**
   * Within the scope of the call, this session is forced into
   * statelessness.  This allows for certain URLs in on the site
   * to be stateless and not generate a session, but if a valid
   * session is presented, they have the scope of that session/User
   */
  def doAsStateless[A](f: => A): A =
    howStateful.doWith(false)(f)
}

/**
 * Sessions that include this trait will not be retained past the current
 * request and will give notifications of failure if stateful features
 * of Lift are accessed
 */
trait StatelessSession extends HowStateful {
  self: TheSession =>

  override def stateful_? = false
}

/**
 * The responseFuture will be satisfied by the original request handling
 * thread when the response has been calculated. Retries will wait for the
 * future to be satisfied in order to return the proper response.
 */
private[http] final case class AjaxRequestInfo(requestVersion: Long,
                                               responseFuture: Future[_],
                                               lastSeen: Long)

/**
 * The TheSession class containg the session state information
 */
class TheSession(val uniqueId: String,
                  val httpSession: Option[HttpSession]) extends HowStateful {
  var restored: Map[String, Any] = Map.empty
  /*
  def sessionHtmlProperties = LiftRules.htmlProperties.session.is.make openOr LiftRules.htmlProperties.default.is.vend

  val requestHtmlProperties: TransientRequestVar[HtmlProperties] =
    new TransientRequestVar[HtmlProperties](sessionHtmlProperties(S.request openOr Req.nil)) {}
*/
  @volatile
  var markedForTermination = false

  @volatile
  private var _running_? = false

  /**
   * Was this session marked for shutdown... if so,
   * don't remark
   */
  @volatile var markedForShutDown_? = false
/*
  private val fullPageLoad = new ThreadGlobal[Boolean] {
    def ? = this.box openOr false
  }
*/
  private val rwl = new java.util.concurrent.locks.ReentrantReadWriteLock()
  /**
   *  ****IMPORTANT**** when you access messageCallback, it *MUST*
   * be in a block that's synchronized on the owner TheSession
   */
  private val nmessageCallback: ConcurrentHashMap[String, R.AFuncHolder] = new ConcurrentHashMap()
  private val nmessageCallbackByRenderV: ConcurrentHashMap[String, List[String]] = new ConcurrentHashMap()

  //@volatile private[http]  var notices: Seq[(NoticeType.Value, NodeSeq, Box[String])] = Nil

  //private val nasyncComponents: ConcurrentHashMap[(Box[String], Box[String]), CometActor] = new ConcurrentHashMap()

  private val nasyncById: ConcurrentHashMap[String, (ActorRef, Long)] = new ConcurrentHashMap()

  private val nmyVariables: ConcurrentHashMap[String, Any] = new ConcurrentHashMap()

  @volatile private var onSessionEnd: List[TheSession => Unit] = Nil

  //private val sessionVarSync = new Object

  /**
  * Cache the value of allowing snippet attribute processing
  */
  //private object allowAttributeProcessing extends TransientRequestVar(LiftRules.allowAttributeSnippets.vend())

  /**
   * A mapping between pages denoted by RenderVersion and
   * functions to execute at the end of the page rendering
   */
  //private var postPageFunctions: Map[String, PostPageFunctions] = Map()

  /**
   * A list of AJAX requests that may or may not be pending for this
   * session. There is an entry for every AJAX request we don't *know*
   * has completed successfully or been discarded by the client.
   *
   * See LiftServlet.handleAjax for how we determine we no longer need
   * to hold a reference to an AJAX request.
   */
  /*
  private var ajaxRequests: ConcurrentHashMap[String,List[AjaxRequestInfo]] = new ConcurrentHashMap()

  def withAjaxRequests[T](fn: (java.util.Map[String, List[AjaxRequestInfo]]) => T) = {
    fn(ajaxRequests)
  }
  * 
  */
  

  /**
   * The synchronization lock for the postPageFunctions
   */
  private val postPageLock = new Object

  @volatile
  private[http] var lastServiceTime: Long = System.currentTimeMillis()
  def getLastServicetime: Long = lastServiceTime

  @volatile
  private[http] var inactivityLength: Long = 30 * 60 * 1000: Long //30 minutes
  def getInactivityLength: Long = inactivityLength

  //private[http] var highLevelSessionDispatcher = new HashMap[String, LiftRules.DispatchPF]()
  //private[http] var sessionRewriter = new HashMap[String, LiftRules.RewritePF]()


  //private object snippetMap extends RequestVar[Map[String, AnyRef]](Map())

  //private[http] object deferredSnippets extends RequestVar[HashMap[String, Box[NodeSeq]]](new HashMap)

  private object cometSetup extends SessionVar[List[((Box[String], Box[String]), Any)]](Nil)


  private var inited = false
  def initDone(): Unit = inited = true
  def isInited: Boolean = inited
  def startSession(): Unit = {
    _running_? = true
    for (sess <- httpSession) {
      // calculate the inactivity length.  If the length is
      // defined in LiftRules and it's less than the container's length
      // then use the Lift length.  Why not use it if the Lift length is
      // longer?  Well, the container's just going to time you out, so
      // why bother.
      inactivityLength = sess.maxInactiveInterval// * 1000L
      /*
        (sess.maxInactiveInterval * 1000L,
          LiftRules.sessionInactivityTimeout.vend) match {
          case (container, Full(lift)) if lift < container => lift
          case (container, _) => container
        }
        * 
        */
    }

    lastServiceTime = System.currentTimeMillis()
    TheSession.onSetupSession.foreach(_(this))
    //sessionHtmlProperties // cause the properties to be calculated
  }

  def running_? : Boolean = _running_?

  //private var cometList: Vector[(CometActor, HttpRequest)] = Vector.empty
  /*

  // Returns a 2-tuple: _1 is a list of valid (LiftActor, Req) pairs for
  // this session that match the given hostAndPath, while _2 is a list
  // of invalid (LiftActor, Req) pairs.
  //
  // Invalid pairs are pairs where the hostAndPath lookup for the
  // associated Req fails by throwing an exception. Typically this
  // happens on overloaded containers that leave Reqs with underlying
  // HttpServletRequests that have expired; these will then throw
  // NullPointerExceptions when their server name or otherwise are
  // accessed.
  def cometForHost(hostAndPath: String): (Vector[(LiftActor, Req)], Vector[(LiftActor, Req)]) =
    synchronized {
      cometList
    }.foldLeft((Vector[(LiftActor, Req)](), Vector[(LiftActor, Req)]())) {
      (soFar, current) =>
        (soFar, current) match {
          case ((valid, invalid), pair @ (_, r)) =>
            try {
              if (r.hostAndPath == hostAndPath)
                (valid :+ pair, invalid)
              else
                soFar
            } catch {
              case exception: Exception =>
                (valid,  invalid :+ pair)
            }
        }
    }

  private[http] def enterComet(what: (LiftActor, Req)): Unit = synchronized {
    LiftRules.makeCometBreakoutDecision(this, what._2)
    if (!running_?) what._1 ! BreakOut()
    cometList = cometList :+ what
  }

  private[http] def exitComet(what: LiftActor): Unit = synchronized {
    cometList = cometList.filterNot(_._1 eq what)
  }
  * 
  */

  private case class RunnerHolder(name: String, func: R.AFuncHolder, owner: Option[String])

  /*
  object ieMode extends SessionVar[Boolean]({
    (for (r <- R.request) yield r.isIE6 || r.isIE7 ||
          r.isIE8) openOr true
  }) {
    //override private[liftweb] def magicSessionVar_? = true
  }
  * 
  */

  def terminateHint(): Unit = {
    if (_running_?) {
      markedForTermination = true
    }
  }


  /**
   * Find a function in the function lookup table.  You probably never need to do this, but
   * well, you can look them up.
   */
  def findFunc(funcName: String): Option[R.AFuncHolder] = {
    rwl.readLock().lock()
    val res = Box !! nmessageCallback.get(funcName)
    rwl.readLock().unlock()
    res
  }

  
  // snapshot for ajax calls
  def makeRequestVarsSnapshot(): Unit = {
    val rv = RenderVersion.get
    rwl.writeLock().lock()
    nmessageCallbackByRenderV.put(rv, rv :: Option(nmessageCallbackByRenderV.get(rv)).getOrElse(Nil))
    nmessageCallback.put(rv, R.PageStateHolder(Full(rv), this))
    //Logger.debug("makeRequestVarsSnapshot done")
    rwl.writeLock().unlock()
  }

  /**
   * Executes the user's functions based on the query parameters
   */
  def runParams(state: HttpRequest): Seq[Any] = {
    //Logger.info("runParams: " + state.path + " : " + state.paramNames + " : " + state.files.keys.toSeq)

    //TODO: optimize (no pattern matching)
    val now = System.currentTimeMillis()
    val toRun = {
      // get all the commands, sorted by owner,
      (state.files.keys.toSeq ++ state.paramNames).distinct.
        flatMap {
        n => {
          //Logger.info("getting params: " + n + " : " + nmessageCallback)
          (Box !! nmessageCallback.get(n)).map(mcb => RunnerHolder(n, mcb, mcb.owner))
        }
      }.sortWith {
        case (RunnerHolder(_, _, Some(a)), RunnerHolder(_, _, Some(b))) if a < b => true
        case (RunnerHolder(_, _, Some(a)), RunnerHolder(_, _, Some(b))) if a > b => false
        case (RunnerHolder(an, _, Some(a)), RunnerHolder(bn, _, Some(b))) if a == b => an < bn
        case (RunnerHolder(_, _, Some(_)), _) => false
        case (_, RunnerHolder(_, _, Some(_))) => true
        case (RunnerHolder(a, _, _), RunnerHolder(b, _, _)) => a < b
        case _ => false
      }
    }

    Logger.info("toRun: " + toRun)

    def buildFunc(i: RunnerHolder): () => Any = i.func match {
      case bfh if bfh.supportsFileParams_? =>
        () => state.files.get(i.name).map(bfh(_))
      case normal =>
        () => {
          val p = state.params.get(i.name).toList
          normal(p) 
        }
          /*
          normal(state.params.getOrElse(i.name,
          Nil/*state.uploadedFiles.filter(_.name == i.name).map(_.fileName)*/))
          * 
          */
    }
    
    //optimize (лишние проходы по циклам)
    val ret = toRun.map(_.owner).distinct.flatMap {
      w =>
        val f = toRun.filter(_.owner == w)
        
        f.map(i => buildFunc(i).apply())

        /*
        w match {
          // if it's going to a CometActor, batch up the commands
          case Some(id) if nasyncById.contains(id) => (Box !! nasyncById.get(id)).toList.flatMap(a =>
            a.!?(ActionMessageSet(f.map(i => buildFunc(i)), state)) match {
              case li: List[_] => li
              case other => Nil
            })
          case _ => Nil//f.map(i => buildFunc(i).apply())
        }
        *
        */

    }

    ret
  }

  /**
   * Updates the internal functions mapping
   */
  def updateFunctionMap(funcs: Map[String, R.AFuncHolder], uniqueId: String, when: Long): Unit = {
    Logger.debug("updateFunctionMap")
    funcs.foreach {
      case (name, func) =>
        rwl.writeLock().lock()
        Logger.debug("updateFunctionMap: " + name)
        nmessageCallbackByRenderV.put(uniqueId, name :: Option(nmessageCallbackByRenderV.get(uniqueId)).getOrElse(Nil))
        nmessageCallback.put(name,
          if (func.owner.contains(uniqueId)) func else func.duplicate(uniqueId))
        rwl.writeLock().unlock()
    }
  }

  def removeFunction(name: String): Unit = {
    rwl.writeLock().lock()
    Option(nmessageCallback.get(name)).foreach(_.owner.foreach(rv => {
      val l = nmessageCallbackByRenderV.get(rv).filter(_ != name)
      if (l.nonEmpty)
        nmessageCallbackByRenderV.put(rv, l)
      else
        nmessageCallbackByRenderV.remove(rv)
    }))
    nmessageCallback.remove(name)
    rwl.writeLock().unlock()
  }


  /**
   * Set your session-specific progress listener for mime uploads
   *     pBytesRead - The total number of bytes, which have been read so far.
   *    pContentLength - The total number of bytes, which are being read. May be -1, if this number is unknown.
   *    pItems - The number of the field, which is currently being read. (0 = no item so far, 1 = first item is being read, ...)
   */
  var progressListener: Box[(Long, Long, Int) => Unit] = Empty

  /**
   * Called just before the session exits.  If there's clean-up work, override this method
   */
  private[http] def cleanUpSession(): Unit = {
    nmessageCallbackByRenderV.clear()
    nmessageCallback.clear()
    //notices = Nil
    //nasyncComponents.clear
    if (!nasyncById.isEmpty) {
      nasyncById.values().asScala.foreach(_._1 ! PoisonPill)
    }
    nasyncById.clear()
    nmyVariables.clear()
    onSessionEnd = Nil
    //postPageFunctions = Map()
    //highLevelSessionDispatcher = HashMap.empty
    //sessionRewriter = HashMap.empty
  }

  def touchSession(): Unit = lastServiceTime = System.currentTimeMillis()
  /*
  def fixSessionTime(): Unit = synchronized {
    for (httpSession <- this.httpSession) {
      lastServiceTime = System.currentTimeMillis() // DO NOT REMOVE THIS LINE!!!!!
      val diff = lastServiceTime - httpSession.lastAccessedTime
      val maxInactive = httpSession.maxInactiveInterval.toInt
      val togo: Int = maxInactive - (diff / 1000L).toInt
      // if we're within 2 minutes of session timeout and
      // the Servlet session doesn't seem to have been updated,
      // extends the lifespan of the HttpSession
      if (diff > 1000L && togo < 120) {
        httpSession.setMaxInactiveInterval(maxInactive + 120)
      }
    }
  }
  * 
  */

  def doCometActorCleanup(): Unit = {
    /*
    val acl =
      this.nasyncComponents.values.toList

    acl.foreach(_ ! ShutdownIfPastLifespan)
    * 
    */
  }

  /**
   * Adds a cleanup function that will be executed when session is terminated
   */
  def addSessionCleanup(f: TheSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  def doShutDown(): Unit = {
    if (running_?) {
      this.shutDown()
    }
  }

  /**
   * Puts the correct thread locking around access to postPageFunctions
   */
  private def accessPostPageFuncs[T](f: => T): T = {
    postPageLock.synchronized {
      f
    }
  }

  def cleanupUnseenFuncs(): Unit = {
    Logger.debug("cleanupUnseenFuncs: " + stateful_?)
    if (stateful_?) {
      val now = System.currentTimeMillis()
      nmessageCallback.entrySet().asScala.foreach { s =>
        val k = s.getKey
        val f = s.getValue

        //Logger.debug("cleanup functions: " + k + " : " + (now - f.lastSeen) + " : " + TheSession.unusedFunctionsLifeTime)

        if (!f.sessionLife &&
          f.owner.isDefined &&
          (now - f.lastSeen) > TheSession.unusedFunctionsLifeTime) {
          rwl.writeLock().lock()
          val rv = f.owner.get
          nmessageCallback.remove(k)

          val l = nmessageCallbackByRenderV.get(rv).filter(_ != k)
          if (l.nonEmpty)
            nmessageCallbackByRenderV.put(rv, l)
          else
            nmessageCallbackByRenderV.remove(rv)

          rwl.writeLock().unlock()
        }
      }

      //Убиваем старые кометы привязанные к страничке, которую давно никто не просматривал
      nasyncById.entrySet().asScala.foreach { e =>
        val v = e.getValue
        if (now - v._2 > TheSession.unusedFunctionsLifeTime) {
          Logger.debug("Убиваем старые комет-акторы")
          v._1 ! PoisonPill
          nasyncById.remove(e.getKey)
        }
      }
    }
  }

  /**
   * Updates the timestamp of the functions owned by this owner and return the
   * number of updated functions
   */
  def updateFuncAndCometByOwner(ownerName: String): Int = {
    val time = System.currentTimeMillis()
    val funcs = Option(nmessageCallbackByRenderV.get(ownerName)).getOrElse(Nil)

    funcs.foreach(fn => {
      Option(nmessageCallback.get(fn)).foreach(f => f.lastSeen = time)
    })
    
    funcs.size
  }

  private def shutDown(): Unit = {
    var done: List[() => Unit] = Nil

    R.initIfUninitted(this) {
      onSessionEnd.foreach(_(this))
      TheSession.onAboutToShutdownSession.foreach(_(this))

      _running_? = false

      cleanUpSession()
      TheSession.onShutdownSession.foreach(f => done ::= (() => f(this)))
    }

    done.foreach(_.apply())
  }

  /**
   * Destroy the current session, then create a new session and
   * continue the execution of the code.  The continuation function
   * must return Nothing (it must throw an exception... this is typically
   * done by calling S.redirectTo(...)).  This method is
   * useful for changing sessions on login.  Issue #727.
   */
  def destroySessionAndContinueInNewSession(continuation: () => Nothing): Nothing = {
    throw new ContinueResponseException(continuation)
  }

  /**
   * Set a session-local variable to a value
   *
   * @param name -- the name of the variable
   * @param value -- the value of the variable
   */
  private[http] def set[T](name: String, value: T): Unit = {
    nmyVariables.put(name , value)
  }

  /**
   * Gets the named variable if it exists
   *
   * @param name -- the name of the session-local variable to get
   *
   * @return Full ( value ) if found, Empty otherwise
   */
  private[http] def get[T](name: String): Option[T] =
    Option(nmyVariables.get(name).asInstanceOf[T])


  /**
   * Unset the named variable
   *
   * @param name the variable to unset
   */
  private[http] def unset(name: String): Unit = {
    nmyVariables.remove(name)
  }

  /**
   * During the HTTP request/response cycle or in a CometActor,
   * Lift populates "S" with information about the current session,
   * the current request, etc.  This method allows you to wrap a
   * function in another function that will snapshot current state
   * (request vars, Req, Loc, etc.) such that when the returned
   * function is executed, it will be executed as if it had been
   * executed in the scope of the thread where it was create.
   * This allows you to farm work out to separate threads, but make
   * it look to those threads as if the scope was the same as if it
   * had been executed on the thread that created the function.
   */
  def buildDeferredFunction[T](f: () => T): () => T = {
    val currentReq: Box[HttpRequest] = R.request.map(_.snapshot)

    val renderVersion = RenderVersion.get

    //val currentMap = snippetMap.is
    //val curLoc = R.location

    val requestVarFunc = RequestVarHandler.generateSnapshotRestorer[T]()

    () => {
      requestVarFunc(() =>
        executeInScope(currentReq, renderVersion)(f()))
    }
  }

  def executeInScope[T](req: Option[HttpRequest], renderVersion: String)(f: => T): T = {
    def doExec(): T = {
      RenderVersion.doWith(renderVersion) {
      try {
        f
      } finally {
        if (R.functionMap.nonEmpty) {
          this.updateFunctionMap(R.functionMap,
            renderVersion, System.currentTimeMillis())
          R.clearFunctionMap()
        }
      }
      }
    }

    req match {
      case Some(r) => R.init(r, this)(doExec())
      case _ => R.initIfUninitted(this)(doExec())
    }
  }

  /**
   * Run the code, but if the session is not stateful, then
   * throw a StateInStatelessException
   */
  def testStatefulFeature[T](f: => T): T = {
    if (this.stateful_?) f
    else throw new IllegalStateException(
      "Accessing stateful feature outside of a stateful session")
  }

  /**
   * Finds a Comet actor by ID
   */
  def getAsyncComponent(id: Option[String]): Seq[ActorRef] =
    testStatefulFeature(
      id.map(i => Option(nasyncById.get(i)).toSeq).getOrElse(nasyncById.values().asScala.toSeq).map(_._1)
    )
  def withAsyncComponent(id: String, func: ActorRef => Unit): Unit = {
    getAsyncComponent(Some(id)).headOption match {
      case Some(comet_actor) => func(comet_actor)
      case _ =>
        subscribeToAsyncComponent(id, func)
    }
  }
  private val subscribedToAsyncComponents: ConcurrentHashMap[String, ActorRef => Unit] = new ConcurrentHashMap()
  private def subscribeToAsyncComponent(id: String, func: ActorRef => Unit) = {
    subscribedToAsyncComponents.put(id, func)
  }

  private class RoundTripActor extends CometActor {
    override def receive: Receive = roundrip orElse super.receive
		        
    private def roundrip: Receive = {
      case ItemMsgStr(guid, value) =>
        Logger.debug("Got ItemMsgStr")
        sendObj("""{"rtguid": "%s", "data": %s}""".format(guid, value))
      case ItemMsg(guid, value) =>
        Logger.debug("Got ItemMsg")
        sendObj("""{"rtguid": "%s", "data": %s}""".format(guid, value))
      case DoneMsg(guid) =>
        Logger.debug("Got DoneMsg")
        sendJson("rtdone", "\"%s\"".format(guid))
      case FailMsg(guid, msg) => sendObj("""{"rtfail": "%s", "msg": "%s"}""".format(guid, msg.replaceAll("""([\\\"])""", """\$1""")))
    }
  }
  /**
   * Build a bunch of round-trip calls between the client and the server.
   * The client calls the server with a parameter and the parameter gets
   * marshalled to the server and the code is executed on the server.
   * The result can be an item (JValue) or a Stream of Items.
   *
   * If the
   * The // HERE
   */
  def buildRoundtrip(info: Seq[RoundTripInfo]): String = {
    testStatefulFeature{
    	val renderVersion = RenderVersion.get

      Logger.debug("buildRoundtrip: " + renderVersion)

      /*
      try {
        throw new Exception("Check")
      } catch {
        case e: Throwable => e.printStackTrace()
      }
       */
    	
    	val ca = Option(nasyncById.get(renderVersion)).map(_._1) match {
    	  case Some(ca) =>
          ca
    	  case _ =>
          import akka.actor.Props
          val ca = CometContext.system.actorOf(Props(new RoundTripActor), renderVersion)
          Logger.debug("Create roundtrip: " + renderVersion + " : " + nasyncById.get(renderVersion) + " : " + ca)
          nasyncById.put(renderVersion, (ca, System.currentTimeMillis()))

          Option(subscribedToAsyncComponents.get(renderVersion)).foreach(f => {
            f(ca)
            subscribedToAsyncComponents.remove(renderVersion)
          })

          ca
      }
    	
      //ca.callInitCometActor(this, Full(Helpers.nextFuncName), Full(Helpers.nextFuncName), NodeSeq.Empty, Map.empty)

      //implicit val defaultFormats = DefaultFormats

      //ca ! PerformSetupComet2(Empty)

      //val node: Elem = ca.buildSpan(ca.renderClock, NodeSeq.Empty)

      //R.addCometAtEnd(node)

      val currentReq: Option[HttpRequest] = R.request.map(_.snapshot)

      //val renderVersion = RenderVersion.get

      val jvmanifest: Manifest[JsonNode] = implicitly

      val map = Map(info.map(i => i.name -> i) :_*)

      def fixIt(in: Any): String = {
        in match {
          case jv: JsonNode => jv.textValue()
          case a => Jckson.serialize(a)
        }
      }

      def localFunc(in: JsonNode): String = {
        import CometContext.executionContext
        Future {
          executeInScope(currentReq, renderVersion){
            val guid = in.get("guid").asText()
            val name = in.get("name").asText()

            val payload = in.get("payload")
            for {
              func <- map.get(name)
              reified <- if (func.manifest == jvmanifest) Some(payload) else {
                Logger.error("Failed to extract "+payload+" as "+func.manifest)
                ca ! FailMsg(guid, "Failed to extract payload as "+func.manifest)
                None
              }
            } {
              func match {
                case StreamRoundTrip(_, func) =>
                  try {
                    for (v <- func.asInstanceOf[Function1[Any, Stream[Any]]](reified)) {
                      v match {
                        //case jsCmd: JsCmd => ca ! jsCmd
                        case jsExp: String => ca ! ItemMsgStr(guid, jsExp)
                        case v => ca ! ItemMsgStr(guid,fixIt(v))
                      }
                    }
                    ca ! DoneMsg(guid)
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

                case SimpleRoundTrip(_, func) =>
                  try {
                    func.asInstanceOf[Function1[Any, Any]](reified ) match {
                      //case jsCmd: JsCmd => ca ! jsCmd
                      case jsExp: String => ca ! ItemMsgStr(guid, jsExp)
                      case v => ca ! ItemMsgStr(guid, fixIt(v))
                    }
                    ca ! DoneMsg(guid)
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

                case HandledRoundTrip(_, func) =>
                  try {
                    func.asInstanceOf[Function2[Any, RoundTripHandlerFunc, Unit]](reified, new RoundTripHandlerFunc {
                      @volatile private var done_? = false
                      def done() {
                        if (!done_?) {
                          done_? = true
                          ca ! DoneMsg(guid)
                        }
                      }

                      def failure(msg: String) {
                        if (!done_?) {
                          done_? = true
                          ca ! FailMsg(guid, msg)
                        }
                      }


                      /**
                       * Send some JavaScript to execute on the client side
                       * @param value
                       */
                      def send(value: String): Unit = {
                        if (!done_?) {
	                        ca ! CometUpdateEval(value)
	                      }
                      }

                      def send(value: JsonNode): Unit = {
                        if (!done_?) {
                          ca ! ItemMsg(guid, value)
                        }
                      }

                      def sendJVal(value: String): Unit = {
                        if (!done_?) {
                          ca ! ItemMsgStr(guid, value)
                        }
                      }
                    })
                  } catch {
                    case e: Exception => ca ! FailMsg(guid, e.getMessage)
                  }

              }
            }
          }
        }
        ""
      }


      lazy val theFunc = "function(v) {%s}".format(jsonCall("v", localFunc)._2)

      lazy val build: (String, String) = "_call_server" -> theFunc

      val objL = (build :: info.map(info => info.name -> 
        """
          |function(param) {
          |  var promise = new mwAjax.Promise();
          |  this._call_server({guid: promise.guid, name: "%s", payload: param});
          |  return promise;
          |}
          |""".format(info.name).stripMargin).toList).map(v => {
            "\"%s\": %s".format(v._1, v._2)
          }).reduce("%s, %s".format(_, _))
      "{%s}".format(objL)
    }
  }

  private case class ItemMsg(guid: String, item: JsonNode)
  private case class ItemMsgStr(guid: String, itemJson: String)
  private case class DoneMsg(guid: String)
  private case class FailMsg(guid: String, msg: String)

    
  /**
   * Build a JavaScript function that will perform a JSON call based on a value calculated in JavaScript.
   * This method uses the Lift-JSON package rather than the old, slow, not-typed JSONParser.  This is the preferred
   * way to do client to server JSON calls.
   *
   * @param jsCalcValue the JavaScript to calculate the value to be sent to the server
   * @param func the function to call when the data is sent
   *
   * @return the function ID and JavaScript that makes the call
   */
  def jsonCall(jsCalcValue: String, func: JsonNode => String): (String, String) =
    jsonCall_*(jsCalcValue, R.SFuncHolder(s => func(Jckson.parse(s))))
    
  /**
   * Build a JavaScript function that will perform an AJAX call based on a value calculated in JavaScript
   * @param jsCalcValue -- the JavaScript to calculate the value to be sent to the server
   * @param func -- the function to call when the data is sent
   * 
   * @return the function ID and JavaScript that makes the call
   */
  private def jsonCall_*(jsCalcValue: String, func: R.AFuncHolder): (String, String) =
    R.fmapFunc(func)(name =>
            (name,
                makeAjaxCall("'%s=' + encodeURIComponent(JSON.stringify(%s))".format(name, jsCalcValue))
            ))
  
  /**
   * Invokes the Ajax request
   * @param in the JsExp that returns the request data
   */
  def makeAjaxCall(in: String): String = "mwAjax.ajax(%s)".format(in)
}

/**
 * Stuff related to round trip messages
 */
sealed trait RoundTripInfo {
  def name: String
  def manifest: Manifest[_]
}

/**
 * The companion objects. Has tasty implicits
 */
object RoundTripInfo {
  implicit def streamBuilder[T](in: (String, T => Stream[Any]))(implicit m: Manifest[T]): RoundTripInfo =
  StreamRoundTrip(in._1, in._2)(m)

  implicit def simpleBuilder[T](in: (String, T => Any))(implicit m: Manifest[T]): RoundTripInfo =
    SimpleRoundTrip(in._1, in._2)(m)

  implicit def handledBuilder[T](in: (String, (T, RoundTripHandlerFunc) => Unit))(implicit m: Manifest[T]): RoundTripInfo =
    HandledRoundTrip(in._1, in._2)(m)
}

/**
 * A function (well, an interface with a bunch of methods on it) to call
 * depending on the state of the round trip function.
 */
trait RoundTripHandlerFunc {
  /**
   * Send data back to the client. This may be called
   * many times and each time, more data gets sent back to the client.
   * @param value the data to send back.
   */
  def send(value: JsonNode): Unit

  /**
   * Send some JavaScript to execute on the client side
   * @param value
   */
  def send(value: String): Unit

  /**
   * Send data (json string) back to the client. This may be called
   * many times and each time, more data gets sent back to the client.
   * @param value the data to send back.
   */
  def sendJVal(value: String): Unit

  /**
   * When you are done sending data back to the client, call this method
   */
  def done(): Unit

  /**
   * If there's a failure related to the computation, call this method.
   * @param msg
   */
  def failure(msg: String): Unit
}

final case class StreamRoundTrip[T](name: String, func: T => Stream[Any])(implicit val manifest: Manifest[T]) extends RoundTripInfo
final case class SimpleRoundTrip[T](name: String, func: T => Any)(implicit val manifest: Manifest[T]) extends RoundTripInfo
final case class HandledRoundTrip[T](name: String, func: (T, RoundTripHandlerFunc) => Unit)(implicit val manifest: Manifest[T]) extends RoundTripInfo


class ContinueResponseException(continuation: () => Nothing) extends Exception("")