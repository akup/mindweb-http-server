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

package com.nn
package http

import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorRef
import com.fasterxml.jackson.databind._
import com.nn.http.vars._
import net.aklabs.helpers.Helpers._
import net.aklabs.helpers.JsonHelpers.Jckson
import net.aklabs.helpers._
import org.pmw.tinylog.Logger

import scala.concurrent.Future
import scala.collection.JavaConverters._

object TheSession {

  val unusedFunctionsLifeTime: Int = 30 * 60 * 1000 // 10 vinutes
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

  //override def allowContainerState_? = false
}


/*
/**
 * Sessions that include this trait will only have access to the container's
 * state via ContainerVars.  This mode is "migratory" so that a session
 * can migrate across app servers.  In this mode, functions that
 * access Lift state will give notifications of failure if stateful features
 * of Lift are accessed
 */
trait MigratorySession extends HowStateful {
  self: TheSession =>

  override def stateful_? = false
}

/**
 * Keeps information around about what kinds of functions are run
 * at the end of page rendering.  The results of these functions will be
 * appended to the bottom of the page.
 *
 * @param renderVersion -- the page ID (aka the RenderVersion)
 * @param functionCount -- the number of functions in the collection
 * @param lastSeen -- page of the page-level GC
 * @param functions -- the list of functions to run
 */
private final case class PostPageFunctions(renderVersion: String,
                                           functionCount: Int,
                                           longLife: Boolean,
                                           lastSeen: Long,
                                           functions: List[() => JsCmd]) {
  /**
   * Create a new instance based on the last seen time
   */
  def updateLastSeen = new PostPageFunctions(renderVersion,
    functionCount,
    longLife,
    System.currentTimeMillis(),
    functions)


}
*/
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

  private val nasyncById: ConcurrentHashMap[String, ActorRef] = new ConcurrentHashMap()

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
  def initDone() = inited = true
  def isInited = inited
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

  def running_? = _running_?

  //private var cometList: Vector[(CometActor, HttpRequest)] = Vector.empty

  def breakOutComet(): Unit = {
    //cometList.foreach(_._1 ! BreakOut())
  }
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

  def terminateHint() {
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
  private[http] def cleanUpSession() {
    nmessageCallbackByRenderV.clear()
    nmessageCallback.clear()
    //notices = Nil
    //nasyncComponents.clear
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
    
    ""
  }

  /**
   * Adds a cleanup function that will be executed when session is terminated
   */
  def addSessionCleanup(f: TheSession => Unit): Unit = synchronized {
    onSessionEnd = f :: onSessionEnd
  }

  /*
  /**
   * Destroy this session and the underlying container session.
   */
  def destroySession() {
    SessionMaster.removeSession(this.uniqueId)

    R.request.foreach(_.session.foreach(_.terminate()))
    this.doShutDown()
  }
  * 
  */

  def doShutDown() {
    if (running_?) {
      // only deal with comet on stateful sessions
      // stateless temporary sessions bar comet use
      if (stateful_?) {
        if (false/*cometList.length > 0*/) {
          this.breakOutComet()
          //Thread.sleep(100)
        }
      }
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
    //Logger.debug("cleanupUnseenFuncs: " + stateful_?)
    if (stateful_?) {
      val now = System.currentTimeMillis()
/*
      accessPostPageFuncs {
        for {
          (key, pageInfo) <- postPageFunctions
        } if (!pageInfo.longLife &&
          (now - pageInfo.lastSeen) > LiftRules.unusedFunctionsLifeTime) {
          postPageFunctions -= key
        }
      }
*/
      
      /*
      withAjaxRequests { currentAjaxRequests =>
        for {
          (version, requestInfos) <- currentAjaxRequests
        } {
          Logger.debug("cleanup ajax: " + version + " : " + requestInfos)
          val remaining =
            requestInfos.filter { info =>
              (now - info.lastSeen) <= TheSession.unusedFunctionsLifeTime
            }

          if (remaining.length > 0)
            currentAjaxRequests.put(version, remaining)
          else
            currentAjaxRequests.remove(version)
        }
      }
      * 
      */

      
        nmessageCallback.entrySet().asScala.foreach {
          s =>
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

    }
  }

  /**
   * Clear the PostPage JavaScript functions for the current page.
   * This is used by CometActor to remove the PostPage JavaScript
   * functions from the given component during redraw.
   */
  /*
  def clearPostPageJavaScriptForThisPage() {
    testStatefulFeature {
      accessPostPageFuncs {
        val rv: String = RenderVersion.get

        postPageFunctions -= rv
      }
    }
  }
  * 
  */

  /*
  /**
   * Associate a function that renders JavaScript with the current page.
   * This function will be run and the resulting JavaScript will be appended
   * to any rendering associated with this page... the normal page render,
   * Ajax calls, and even Comet calls for this page.
   *
   * @param func -- the function that returns JavaScript to be appended to
   * responses associated with this page
   */
  def addPostPageJavaScript(func: () => JsCmd) {
    testStatefulFeature {
      accessPostPageFuncs {
        // The page or cometactor that the functions are associated with
        val rv: String = RenderVersion.get

        val old =
          postPageFunctions.getOrElse(rv,
            PostPageFunctions(rv,
              0,
              S.currentCometActor.
                isDefined,
              Helpers.millis,
              Nil))

        val updated = PostPageFunctions(old.renderVersion,
          old.functionCount + 1,
          old.longLife,
          Helpers.millis,
          func :: old.functions)

        postPageFunctions += (rv -> updated)
      }
    }
  }

  def postPageJavaScript(rv: String): List[JsCmd] = {
    def org = accessPostPageFuncs {
      val ret = postPageFunctions.get(rv)
      ret.foreach {
        r => postPageFunctions += (rv -> r.updateLastSeen)
      }
      ret
    }

    org match {
      case None => Nil
      case Some(ppf) => {
        val lb = new ListBuffer[JsCmd]

        def run(count: Int, funcs: List[() => JsCmd]) {
          funcs.reverse.foreach(f => lb += f())
          val next = org.get // safe to do get here because we know the
          // postPageFunc is defined

          val diff = next.functionCount - count

          // if the function table is updated, make sure to get
          // the additional functions
          if (diff == 0) {} else {
            run(next.functionCount, next.functions.take(diff))
          }
        }

        run(ppf.functionCount, ppf.functions)

        lb.toList

      }
    }
  }

  /**
   * Get the post-page JavaScript functions for a sequence of page IDs.
   * This is used by the CometActor to get the post-page JavaScript functions
   * for the comet actor and for the page the the comet actor is associated with
   */
  def postPageJavaScript(pageIds: Seq[String]): List[JsCmd] = {
    for {
      rv <- pageIds.toList.distinct
      js <- postPageJavaScript(rv)
    } yield js
  }

  /**
   * Get the JavaScript to execute as part of the current page
   */
  def postPageJavaScript(): List[JsCmd] = postPageJavaScript(RenderVersion.get)
  * 
  */

  /**
   * Updates the timestamp of the functions owned by this owner and return the
   * number of updated functions
   */
  def updateFuncByOwner(ownerName: String, time: Long): Int = {
    /*
    accessPostPageFuncs {
      for {
        funcInfo <- postPageFunctions.get(ownerName)
      } postPageFunctions += (ownerName -> funcInfo.updateLastSeen)
    }
    * 
    */
    //Logger.debug("updateFuncByOwner")
/*
    withAjaxRequests { currentAjaxRequests =>
      nullTest(currentAjaxRequests.get(ownerName)).foreach { requestInfos =>
        val updated = requestInfos.map(_.copy(lastSeen = time))

        currentAjaxRequests.put(ownerName, updated)
      }
    }
    * 
    */

    val funcs = Option(nmessageCallbackByRenderV.get(ownerName)).getOrElse(Nil)
    
    //Logger.debug("updateFuncByOwner: " + funcs + " : " + ownerName + " : " + nmessageCallbackByRenderV.keySet().toList)
    
    funcs.foreach(fn => {
      Option(nmessageCallback.get(fn)).foreach(f => f.lastSeen = time)
    })
    
    funcs.size
/*
    import scala.collection.JavaConversions._

      (0 /: nmessageCallback.entrySet())((l, v) => l + (v.getValue.owner match {
        case Some(owner) if (owner == ownerName) =>
          v.getValue.lastSeen = time
          1
        case None => v.getValue.lastSeen = time
        1
        case _ => 0
      }))
*/
  }

  /*
  /**
   * Returns true if there are functions bound for this owner
   */
  private[http] def hasFuncsForOwner(owner: String): Boolean = {
   import scala.collection.JavaConversions._

    !nmessageCallback.values().find(_.owner == owner).isEmpty
  }
  * 
  */


  private def shutDown(): Unit = {
    var done: List[() => Unit] = Nil

    R.initIfUninitted(this) {
      onSessionEnd.foreach(_(this))
      //this.synchronized {
      TheSession.onAboutToShutdownSession.foreach(_(this))

      _running_? = false

      //SessionMaster.removeSession(this.uniqueId)

      /*
      nasyncComponents.values().foreach {
        case comp => done ::= (() => tryo(comp ! ShutDown))
      }
      * 
      */
      cleanUpSession()
      TheSession.onShutdownSession.foreach(f => done ::= (() => f(this)))
      //}
    }

    done.foreach(_.apply())
  }

  private object overrideResponseCode extends TransientRequestVar[Box[Int]](Empty)

  /**
   * If the sitemap entry for this Req is marked stateless,
   * run the rest of the request as stateless
   */
  /*
  private def checkStatelessInSiteMap[T](req: HttpRequest)(f: => T): T = {
    req.location match {
      case Some(loc) if loc.stateless_? => this.doAsStateless(f)
      case _ => f
    }
  }
  * 
  */

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

  
  /*
  private[http] def processRequest(request: HttpRequest,
                                   continuation: Box[() => Nothing]): Box[LiftResponse] = {
    val t = System.currentTimeMillis()
    ieMode.is // make sure this is primed
    S.oldNotices(notices)
    TheSession.onBeginServicing.foreach(f => tryo(f(this, request)))
    val ret = try {
      // run the continuation in the new session
      // if there is a continuation
      continuation match {
        case Full(func) => {
          func()
          S.redirectTo("/")
        }
        case _ => // do nothing
      }

      val sessionDispatch = S.highLevelSessionDispatcher

      val toMatch = request
      NamedPF.applyBox(toMatch, sessionDispatch) match {
        case Full(f) =>
          runParams(request)
          try {
            f() match {
              case Full(r) => Full(checkRedirect(r))
              case _ => LiftRules.notFoundOrIgnore(request, Full(this))
            }
          } finally {
            notices = S.getAllNotices
          }

        case _ =>
          logger.trace("START SERVICING: " + (System.currentTimeMillis() - t))
          RenderVersion.get // touch this early

          runParams(request)
          logger.trace("Continue1: " + (System.currentTimeMillis() - t))

          val early = LiftRules.preAccessControlResponse_!!.firstFull(request)
          logger.trace("Continue2: " + (System.currentTimeMillis() - t))

          // Process but make sure we're okay, sitemap wise
          val response: Box[LiftResponse] = early or (request.testLocation match {
            case Left(true) =>
              logger.trace("Continue3: " + (System.currentTimeMillis() - t))
              checkStatelessInSiteMap(request) {
                cleanUpBeforeRender
                logger.trace("Continue4: " + (System.currentTimeMillis() - t))

                PageName(request.uri + " -> " + request.path)
                val resp = LiftRules.allowParallelSnippets.doWith(() => !Props.inGAE) {
                  (request.location.flatMap(_.earlyResponse) or LiftRules.earlyResponse.firstFull(request)) or
                    (processTemplate({
                      val t = System.currentTimeMillis()
                      val ret = locTemplate
                      logger.trace("LOC TEMPLATE: " + (System.currentTimeMillis() - t))
                      ret
                    }, request, request.path, 200) or
                      request.createNotFound {
                        processTemplate(Empty, request, _, 404)
                      })
                }
                logger.trace("Continue5: " + (System.currentTimeMillis() - t))
                resp
              }

            case Right(Full(resp)) => Full(resp)
            case _ if (LiftRules.passNotFoundToChain) => Empty
            case _ if Props.mode == Props.RunModes.Development =>
              request.createNotFound {
                processTemplate(Empty, request, _, 404)
              } or
                Full(ForbiddenResponse("The requested page was not defined in your SiteMap, so access was blocked.  (This message is displayed in development mode only)"))
            case _ => request.createNotFound {
              processTemplate(Empty, request, _, 404)
            }
          })

          // Before returning the response check for redirect and set the appropriate state.
          response.map(checkRedirect)
      }
    } catch {
      case ContinueResponseException(cre) => throw cre

      case ite: java.lang.reflect.InvocationTargetException if (ite.getCause.isInstanceOf[ResponseShortcutException]) =>
        Full(handleRedirect(ite.getCause.asInstanceOf[ResponseShortcutException], request))

      case rd: net.liftweb.http.ResponseShortcutException => Full(handleRedirect(rd, request))

      case e: LiftFlowOfControlException => throw e

      case e: Exception => S.runExceptionHandlers(request, e)

    }

    TheSession.onEndServicing.foreach(f => tryo(f(this, request, ret)))
    ret
  }
  * 
  */

  /*
  private def cleanUpBeforeRender {
    // Reset the mapping between ID and Style for Ajax notices.
    MsgErrorMeta(new HashMap)
    MsgWarningMeta(new HashMap)
    MsgNoticeMeta(new HashMap)
  }

  private[http] def handleRedirect(re: ResponseShortcutException, request: Req): LiftResponse = {
    if (re.doNotices) notices = S.getAllNotices

    re.response
  }
  * 
  */

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


  /*
  private[http] def attachRedirectFunc(uri: String, f: Box[() => Unit]) = {
    f map {
      fnc =>
        val func: String = {
          val funcName = nextFuncName
          nmessageCallback.put(funcName, R.NFuncHolder(() => {
            fnc()
          }))
          funcName
        }
        Helpers.appendFuncToURL(uri, func + "=_")
    } openOr uri

  }

  private[http] def checkRedirect(resp: LiftResponse): LiftResponse = resp match {
    case RedirectWithState(uri, state, cookies) =>
      state.msgs.foreach(m => S.message(m._1, m._2))
      notices = S.getAllNotices
      RedirectResponse(attachRedirectFunc(uri, state.func), cookies: _*)
    case _ => resp
  }
  * 
  */

  /*
  /**
   * Wrap an AFuncHolder with the current snippet and Loc context so that for Ajax calls, the original snippets,
   * RequestVars and Loc (location) are populated
   *
   * @param f the AFuncHolder that you want to wrap with execution context
   */
  private[http] def contextFuncBuilder(f: R.AFuncHolder): R.AFuncHolder = {
    //val currentMap = snippetMap.is
    val curLoc = R.location

    val requestVarFunc: Function1[Function0[Any], Any] = RequestVarHandler.generateSnapshotRestorer()
    new R.ProxyFuncHolder(f) {
      override def apply(in: List[String]): Any =
        requestVarFunc(() =>
          R.CurrentLocation.doWith(curLoc) {
            snippetMap.doWith(snippetMap.is ++ currentMap) {
              super.apply(in)
            }
          }
        )

      override def apply(in: FileParamHolder): Any =
        requestVarFunc(() =>
          R.CurrentLocation.doWith(curLoc) {
            snippetMap.doWith(snippetMap.is ++ currentMap) {
              super.apply(in)
            }
          }
        )
    }
  }
  * 
  */

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
          R.clearFunctionMap
        }
      }
      }
    }

    req match {
      case Some(r) => R.init(r, this)(doExec())
      case _ => R.initIfUninitted(this)(doExec())
    }
  }

/*
  /**
   * Pass in a LiftActor and get a JavaScript expression (function(x) {...}) that
   * represents an asynchronous Actor message from the client to the server.
   *
   * The Actor should respond to a message in the form of a JsonAST.JValue.
   *
   * This method requires the session be stateful
   *
   * In general, your Actor should be a subclass of ScopedLiftActor because
   * that way you'll have the scope of the current session.
   *
   * @param in the Actor to send messages to.
   *
   * @return a JsExp that contains a function that can be called with a parameter
   *         and when the function is called, the parameter is JSON serialized and sent to
   *         the server
   */
  def clientActorFor(in: LiftActor): JsExp = {
    testStatefulFeature{
      AnonFunc("x",
       SHtml.jsonCall(JsRaw("x"), (p: JsonAST.JValue) => {
        in ! p
        JsCmds.Noop
      }).cmd)
    }
  }

  /**
   * Pass in a LiftActor and get a JavaScript expression (function(x) {...}) that
   * represents an asynchronous Actor message from the client to the server.
   *
   * The Actor should respond to a message in the form of a JsonAST.JValue.
   *
   * This method requires the session be stateful
   *
   * In general, your Actor should be a subclass of ScopedLiftActor because
   * that way you'll have the scope of the current session.
   *
   * @param in the Actor to send messages to.
   * @param xlate a function that will take the JsonAST.JValue and convert it
   *              into a representation that can be sent to the Actor (probably
   *              deserialize it into a case class.) If the translation succeeds,
   *              the translated message will be sent to the actor. If the
   *              translation fails, an error will be logged and the raw
   *              JsonAST.JValue will be sent to the actor
   *
   *
   * @return a JsExp that contains a function that can be called with a parameter
   *         and when the function is called, the parameter is JSON serialized and sent to
   *         the server
   */
  def clientActorFor(in: LiftActor, xlate: JsonAST.JValue => Box[Any]): JsExp = {
    testStatefulFeature{
      AnonFunc("x",
        SHtml.jsonCall(JsRaw("x"), (p: JsonAST.JValue) => {
          in.!(xlate(p) match {
            case Full(v) => v
            case Empty => logger.error("Failed to deserialize JSON message "+p); p
            case Failure(msg, _, _) => logger.error("Failed to deserialize JSON message "+p+". Error "+msg); p
          })
          JsCmds.Noop
        }).cmd)

    }
  }
  * 
  */


  /*
  /**
   * Create a Actor that will take messages on the server and then send them to the client. So, from the
   * server perspective, it's just an Async message send. From the client perspective, they get a function
   * called each time the message is sent from the server.
   *
   * If the message sent to the LiftActor is a JsCmd or JsExp, then the code is sent directly to the
   * client and executed on the client.
   *
   * If the message is a JsonAST.JValue, it's turned into a JSON string, sent to the client and
   * the client calls the function named in the `toCall` parameter with the value.
   *
   * If the message is anything else, we attempt to JSON serialize the message and if it
   * can be JSON serialized, it's sent over the wire and passed to the `toCall` function on the server
   * @return
   */
  def serverActorForClient(toCall: String): LiftActor = {
    testStatefulFeature{
      val ca = new CometActor {
        /**
         * It's the main method to override, to define what is rendered by the CometActor
         *
         * There are implicit conversions for a bunch of stuff to
         * RenderOut (including NodeSeq).  Thus, if you don't declare the return
         * turn to be something other than RenderOut and return something that's
         * coercible into RenderOut, the compiler "does the right thing"(tm) for you.
         * <br/>
         * There are implicit conversions for NodeSeq, so you can return a pile of
         * XML right here.  There's an implicit conversion for NodeSeq => NodeSeq,
         * so you can return a function (e.g., a CssBindFunc) that will convert
         * the defaultHtml to the correct output.  There's an implicit conversion
         * from JsCmd, so you can return a pile of JavaScript that'll be shipped
         * to the browser.<br/>
         * Note that the render method will be called each time a new browser tab
         * is opened to the comet component or the comet component is otherwise
         * accessed during a full page load (this is true if a partialUpdate
         * has occurred.)  You may want to look at the fixedRender method which is
         * only called once and sets up a stable rendering state.
         */
        def render: RenderOut = NodeSeq.Empty



        override def lifespan = Full(LiftRules.clientActorLifespan.vend.apply(this))

        override def hasOuter = false

        override def parentTag = <div style="display: none"/>

        override def lowPriority: PartialFunction[Any, Unit] = {
          case jsCmd: JsCmd => partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(jsCmd, false)))
          case jsExp: JsExp => partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(jsExp.cmd, false)))
          case jv: JsonAST.JValue => {
            val s: String = json.pretty(json.render(jv))
            partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(JsRaw(toCall+"("+s+")").cmd, false)))
          }
          case x: AnyRef => {
            import json._
            implicit val formats = Serialization.formats(NoTypeHints)

            val ser: Box[String] = Helpers.tryo(Serialization.write(x))

            ser.foreach(s => partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(JsRaw(toCall+"("+s+")").cmd, false))))

          }

          case _ => // this will never happen because the message is boxed

        }
      }

          nasyncComponents.put(ca.theType -> ca.name, ca)
          nasyncById.put(ca.uniqueId, ca)

      ca.callInitCometActor(this, Full(Helpers.nextFuncName), Full(Helpers.nextFuncName), NodeSeq.Empty, Map.empty)



      ca ! PerformSetupComet2(Empty)

      val node: Elem = ca.buildSpan(ca.renderClock, NodeSeq.Empty)

      S.addCometAtEnd(node)

      ca
    }
  }
  * 
  */


  /**
   * Run the code, but if the session is not stateful, then
   * throw a StateInStatelessException
   */
  def testStatefulFeature[T](f: => T): T = {
    if (this.stateful_?) f
    else throw new IllegalStateException(
      "Accessing stateful feature outside of a stateful session")
  }

  
  //COMETS
  /*
  /**
   * Finds all Comet actors by type
   */
  def findComet(theType: String): List[CometActor] = {
    import scala.collection.JavaConversions._

    testStatefulFeature {
      nasyncComponents.entrySet().toList.flatMap {v => (v.getKey, v.getValue) match {
        case ((Full(name), _), value) if name == theType => Full(value)
        case _ => Empty
      }}.toList
    }
  }

  /**
   * Find the comet actor by type and name
   */
  def findComet(theType: String, name: Box[String]): Box[CometActor] =  {
    testStatefulFeature {
      Box !! nasyncComponents.get(Full(theType) -> name)
    }
  }


  /**
   * This method will send a message to a CometActor, whether or not
   * the CometActor is instantiated.  If the CometActor already exists
   * in the session, the message will be sent immediately.  If the CometActor
   * is not yet instantiated, the message will be sent to the CometActor
   * as part of setup (@see setupComet) if it is created as part
   * of the current HTTP request/response cycle.
   *
   * @param theType the type of the CometActor
   * @param name the optional name of the CometActor
   * @param msg the message to send to the CometActor
   */
  def sendCometActorMessage(theType: String, name: Box[String], msg: Any) {
    testStatefulFeature {
      findComet(theType, name) match {
        case Full(a) => a ! msg
        case _ => setupComet(theType, name, msg)
      }
    }
  }

  /**
   * Allows you to send messages to a CometActor that may or may not be set up yet
   */
  def setupComet(theType: String, name: Box[String], msg: Any) {
    testStatefulFeature {
      cometSetup.atomicUpdate(v => (Full(theType) -> name, msg) :: v)
    }
  }

  private[liftweb] def findComet(theType: Box[String], name: Box[String],
                                 defaultXml: NodeSeq,
                                 attributes: Map[String, String]): Box[LiftCometActor] = {
    testStatefulFeature {
      val what = (theType -> name)
      val ret = {

        val ret = Box.legacyNullTest(nasyncComponents.get(what)).or({
          theType.flatMap {
            tpe =>
              val ret = findCometByType(tpe, name, defaultXml, attributes)
              ret.foreach(r =>
                {
                  nasyncComponents.put(what, r)
                  nasyncById.put(r.uniqueId, r)
                })
              ret
          }
        })

        ret
      }

      for {
        actor <- ret
        (cst, csv) <- cometSetup.is if cst == what
      } actor ! csv

      cometSetup.atomicUpdate(v => v.filter(_._1 != what))

      ret
    }
  }
*/

  /**
   * Finds a Comet actor by ID
   */
  def getAsyncComponent(id: Option[String]): Seq[ActorRef] =
    testStatefulFeature(
      id.map(i => Option(nasyncById.get(i)).toSeq).getOrElse(nasyncById.values().asScala.toSeq)
    )
  def withAsyncComponent(id: String, func: ActorRef => Unit) = {
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

/*
  /**
   * Adds a new Comet actor to this session
   */
  private[http] def addCometActor(act: CometActor): Unit = {
    testStatefulFeature {
      nasyncById.put(act.uniqueId, act)
    }
  }

  private[liftweb] def addAndInitCometActor(act: CometActor,
                                            theType: Box[String],
                                            name: Box[String],
                                            defaultXml: NodeSeq,
                                            attributes: Map[String, String]) = {
    testStatefulFeature {
      val what = (theType -> name)

        nasyncById.put(act.uniqueId,act)
        nasyncComponents.put(what, act)

      act.callInitCometActor(this, theType, name, defaultXml, attributes)
      act ! PerformSetupComet2(if (act.sendInitialReq_?)
        S.request.map(_.snapshot)
      else Empty)
    }
  }

  /**
   * Remove a Comet actor
   */
  private[http] def removeCometActor(act: LiftCometActor): Unit = {
    testStatefulFeature {
      nasyncById.remove(act.uniqueId)
      nmessageCallback.remove(act.jsonCall.funcId)
      nasyncComponents.remove(act.theType -> act.name)

      val toCmp = Full(act.uniqueId)

      import scala.collection.JavaConversions._

      nmessageCallback.foreach {v => v match {
        case (k, f) =>
          if (f.owner == toCmp) nmessageCallback.remove(k)
      }}

      accessPostPageFuncs {
        postPageFunctions -= act.uniqueId
      }

      val id = Full(act.uniqueId)
      nmessageCallback.keys().foreach {
        k =>
          val f = nmessageCallback.get(k)
          if (f.owner == id) {
            nmessageCallback.remove(k)
          }
      }
    }
  }

  private def findCometByType(contType: String,
                              name: Box[String],
                              defaultXml: NodeSeq,
                              attributes: Map[String, String]): Box[LiftCometActor] = {
    testStatefulFeature {
      val createInfo = CometCreationInfo(contType, name, defaultXml, attributes, this)

      val boxCA: Box[LiftCometActor] = LiftRules.cometCreationFactory.vend.apply(createInfo).map {
        a => a ! PerformSetupComet2(if (a.sendInitialReq_?)
          S.request.map(_.snapshot)
        else Empty);
        a
      } or
        LiftRules.cometCreation.toList.find(_.isDefinedAt(createInfo)).map(_.apply(createInfo)).map {
          a => a ! PerformSetupComet2(if (a.sendInitialReq_?)
            S.request.map(_.snapshot)
          else Empty);
          a
        } or
        (findType[LiftCometActor](contType, LiftRules.buildPackage("comet") ::: ("lift.app.comet" :: Nil)).flatMap {
          cls =>
            tryo((e: Throwable) => e match {
              case e: java.lang.NoSuchMethodException => ()
              case e => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)
            }) {
              val constr = cls.getConstructor()
              val ret = constr.newInstance().asInstanceOf[LiftCometActor]
              ret.callInitCometActor(this, Full(contType), name, defaultXml, attributes)

              ret ! PerformSetupComet2(if (ret.sendInitialReq_?)
                S.request.map(_.snapshot)
              else Empty)
              ret.asInstanceOf[LiftCometActor]
            } or tryo((e: Throwable) => logger.info("Comet find by type Failed to instantiate " + cls.getName, e)) {
              val constr = cls.getConstructor(this.getClass, classOf[Box[String]], classOf[NodeSeq], classOf[Map[String, String]])
              val ret = constr.newInstance(this, name, defaultXml, attributes).asInstanceOf[LiftCometActor];

              ret ! PerformSetupComet2(if (ret.sendInitialReq_?)
                S.request.map(_.snapshot)
              else Empty)
              ret.asInstanceOf[LiftCometActor]
            }
        })
      boxCA.foreach {
        _.setCometActorLocale(S.locale)
      }

      boxCA
    }
  }
*/

  private class RoundTripActor extends CometActor {
    override def receive: Receive = roundrip orElse super.receive
		        
    private def roundrip: Receive = {
	    //case jsExp: String => partialUpdate(JsCmds.JsSchedule(JsCmds.JsTry(jsCmd, false)))

      case ItemMsgStr(guid, value) => sendObj("""{"rtguid": "%s", "data": %s}""".format(guid, value))
      case ItemMsg(guid, value) => sendObj("""{"rtguid": "%s", "data": %s}""".format(guid, value))
        //sendJson(name, jv.textValue())
        //partialUpdate(JsCmds.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'success': ${Printer.compact(JsonAST.render(value))}} )").cmd))
      case DoneMsg(guid) => sendJson("rtdone", "\"%s\"".format(guid))
        //partialUpdate(JsCmds.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'done': true} )").cmd))

      case FailMsg(guid, msg) => sendObj("""{"rtfail": "%s", "msg": "%s"}""".format(guid, msg.replaceAll("""([\\\"])""", """\$1""")))
        //partialUpdate(JsCmds.JsSchedule(JsRaw(s"liftAjax.sendEvent(${guid.encJs}, {'failure': ${msg.encJs} })").cmd))
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
    	
    	val ca = Option(nasyncById.get(renderVersion)) match {
    	  case Some(ca) => ca
    	  case _ =>
          import akka.actor.Props
          val ca = CometContext.system.actorOf(Props(new RoundTripActor), renderVersion)
          nasyncById.put(renderVersion, ca)

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
                /*
                try {Some(payload.extract(defaultFormats, func.manifest))} catch {
                  case e: Exception =>
                    logger.error("Failed to extract "+payload+" as "+func.manifest, e)
                    ca ! FailMsg(guid, "Failed to extract payload as "+func.manifest+" exception "+ e.getMessage)
                    None

                }
                * 
                */
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

                      def send(value: JsonNode) {
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



/*
/**
 * The response from a page saying that it's been rendered
 */
case object ShutDown

/**
 * If a class is to be used as a lift view (rendering from code rather than a static template)
 * and the method names are to be used as "actions", the view must be marked as "InsecureLiftView"
 * because there exists the ability to execute arbitrary methods based on wire content
 */
trait InsecureLiftView

/**
 *  The preferred way to do lift views... implement a partial function that dispatches
 * the incoming request to an appropriate method
 */
trait LiftView {
  implicit def nsToCns(in: NodeSeq): Box[NodeSeq] = Box.legacyNullTest(in)

  def dispatch: PartialFunction[String, () => Box[NodeSeq]]
}

// an object that extracts an elem that defines a snippet
private object SnippetNode {
  private def removeLift(str: String): String =
    str.indexOf(":") match {
      case x if x >= 0 => str.substring(x + 1)
      case _ => str
    }

  private def makeMetaData(key: String, value: String, rest: MetaData): MetaData = key.indexOf(":") match {
    case x if x > 0 => new PrefixedAttribute(key.substring(0, x),
      key.substring(x + 1),
      value, rest)

    case _ => new UnprefixedAttribute(key, value, rest)
  }

  private def pairsToMetaData(in: List[String]): MetaData = in match {
    case Nil => Null
    case x :: xs => {
      val rest = pairsToMetaData(xs)
      x.charSplit('=').map(Helpers.urlDecode) match {
        case Nil => rest
        case x :: Nil => makeMetaData(x, "", rest)
        case x :: y :: _ => makeMetaData(x, y, rest)
      }
    }
  }


  private def isLiftClass(s: String): Boolean =
    s.startsWith("lift:") || s.startsWith("l:")

  private def snippy(in: Elem): Option[(String, MetaData)] =
    ((for {
      cls <- in.attribute("class")
      snip <- cls.text.charSplit(' ').find(isLiftClass)
    } yield snip) orElse in.attribute("lift").map(_.text)
      orElse in.attribute("data-lift").map(_.text)).map {
      snip =>
        snip.charSplit('?') match {
          case Nil => "this should never happen" -> Null
          case x :: Nil => urlDecode(removeLift(x)) -> Null
          case x :: xs => urlDecode(removeLift(x)) -> pairsToMetaData(xs.flatMap(_.roboSplit("[;&]")))
        }
    }

  private def liftAttrsAndParallel(in: MetaData): (Boolean, MetaData) = {
    var next = in
    var par = false
    var nonLift: MetaData = Null

    while (next != Null) {
      next match {
        // remove the lift class css classes from the class attribute
        case up: UnprefixedAttribute if up.key == "class" =>
          up.value.text.charSplit(' ').filter(s => !isLiftClass(s)) match {
            case Nil =>
            case xs => nonLift = new UnprefixedAttribute("class",
              xs.mkString(" "),
              nonLift)
          }

        case p: PrefixedAttribute
          if (p.pre == "l" || p.pre == "lift") && p.key == "parallel"
        => par = true

        case up: UnprefixedAttribute if up.key == "lift" || up.key == "data-lift" => // ignore

        case p: PrefixedAttribute
          if p.pre == "lift" && p.key == "snippet"
        => nonLift = p.copy(nonLift)

        case a => nonLift = a.copy(nonLift)
      }
      next = next.next
    }


    (par, nonLift)
  }


  def unapply(baseNode: Node): Option[(Elem, NodeSeq, Boolean, MetaData, String)] =
    baseNode match {
      case elm: Elem if elm.prefix == "lift" || elm.prefix == "l" => {
        Some((elm, elm.child,
          elm.attributes.find {
            case p: PrefixedAttribute => p.pre == "lift" && (p.key == "parallel")
            case _ => false
          }.isDefined,
          elm.attributes, elm.label))
      }

      case elm: Elem => {
        for {
          (snippetName, lift) <- snippy(elm)
        } yield {
          val (par, nonLift) = liftAttrsAndParallel(elm.attributes)
          val newElm = new Elem(elm.prefix, elm.label,
            nonLift, elm.scope, elm.minimizeEmpty, elm.child: _*)
          (newElm, newElm, par ||
            (lift.find {
              case up: UnprefixedAttribute if up.key == "parallel" => true
              case _ => false
            }.
              flatMap(up => AsBoolean.unapply(up.value.text)) getOrElse
              false), lift, snippetName)

        }
      }

      case _ => {
        None
      }
    }
}
* 
*/

/*
/**
 * A LiftActor that runs in the scope of the current Session, repleat with SessionVars, etc.
 * In general, you'll want to use a ScopedLiftActor when you do stuff with clientActorFor, etc.
 * so that you have the session scope
 *
 */
trait ScopedLiftActor extends LiftActor with LazyLoggable {
  /**
   * The session captured when the instance is created. It should be correct if the instance is created
   * in the scope of a request
   */
  protected val _session: TheSession = R.session openOr new TheSession("", Helpers.nextFuncName, Empty)

  /**
   * The render version of the page that this was created in the scope of
   */
  protected val _uniqueId: String = RenderVersion.get

  /**
   * The session associated with this actor. By default it's captured at the time of instantiation, but
   * that doesn't always work, so you might have to override this method
   * @return
   */
  def session: TheSession = _session


  /**
   * The unique page ID of the page that this Actor was created in the scope of
   * @return
   */
  def uniqueId: String = _uniqueId

  /**
   * Compose the Message Handler function. By default,
   * composes highPriority orElse mediumPriority orElse internalHandler orElse
   * lowPriority orElse internalHandler.  But you can change how
   * the handler works if doing stuff in highPriority, mediumPriority and
   * lowPriority is not enough.
   */
  protected def composeFunction: PartialFunction[Any, Unit] = composeFunction_i

  private def composeFunction_i: PartialFunction[Any, Unit] = {
    // if we're no longer running don't pass messages to the other handlers
    // just pass them to our handlers
    highPriority orElse mediumPriority orElse
      lowPriority
  }

  /**
   * Handle messages sent to this Actor before the
   */
  def highPriority: PartialFunction[Any, Unit] = Map.empty

  def lowPriority: PartialFunction[Any, Unit] = Map.empty

  def mediumPriority: PartialFunction[Any, Unit] = Map.empty

  protected override def messageHandler = {
    val what = composeFunction
    val myPf: PartialFunction[Any, Unit] = new PartialFunction[Any, Unit] {
      def apply(in: Any): Unit =
        R.initIfUninitted(session) {
          RenderVersion.doWith(uniqueId) {
            R.functionLifespan(true) {
              try {
                what.apply(in)
              } catch {
                case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e)
                case e: Exception => reportError("Message dispatch for " + in, e)
              }
              if (S.functionMap.size > 0) {
                session.updateFunctionMap(S.functionMap, uniqueId, millis)
                S.clearFunctionMap
              }
            }
          }
        }

      def isDefinedAt(in: Any): Boolean =
        R.initIfUninitted(session) {
          RenderVersion.doWith(uniqueId) {
            R.functionLifespan(true) {
              try {
                what.isDefinedAt(in)
              } catch {
                case e if exceptionHandler.isDefinedAt(e) => exceptionHandler(e); false
                case e: Exception => reportError("Message test for " + in, e); false
              }
            }
          }
        }
    }

    myPf
  }

  /**
   * How to report an error that occurs during message dispatch
   */
  protected def reportError(msg: String, exception: Exception) {
    Logger.error(msg, exception)
  }
}
* 
*/

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