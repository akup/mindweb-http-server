package com.nn.mindweb.server

import java.net.{InetSocketAddress, URLDecoder}
import akka.util.Timeout
import com.aklabs.login._
import com.aklabs.login.grpc.LoginClient
import com.aklabs.user.{AbstractEdgeUser, EntityUserId}
import com.google.common.base.Splitter
import com.nn.mindweb.server.messages._
import com.nn.mindweb.server.netty.RemoteNettyHttpRequest
import com.nn.mindweb.server.sessiongrpc.SessionClient
import io.netty.handler.codec.DecoderResult
import io.netty.handler.codec.http.{DefaultHttpRequest, HttpHeaders, HttpMethod, HttpVersion}
import net.aklabs.helpers.JsonHelpers.{JObject, Jckson}
import net.aklabs.helpers.TimeHelpers._
import net.aklabs.helpers.{Helpers, _}
import net.aklabs.http.{HttpRequest, HttpSession, MapParamMap, R, SessionInfo, StatelessSession, StreamFileParamHolder, TheSession}
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.collection.{immutable, mutable}
import scala.concurrent.Await
import scala.language.postfixOps
import scala.util.Sorting

object Request {
  def apply() = new Request(RemoteNettyHttpRequest(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, "/")), stateful = false)
  def apply(path: String) = new Request(RemoteNettyHttpRequest(new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path)), stateful = false)
}

class Request(val _request: RemoteNettyHttpRequest, val stateful: Boolean)
  extends HttpRequest with CookieMessage {

  //var multiParams:  Map[String, MultipartItem]  = Map.empty
  private val request = _request.request
  val routeParams:  mutable.Map[String, String]         = mutable.Map.empty
  var error:        Option[Throwable]           = None

  def accepts: Seq[ContentType] = {
    val accept = request.headers.get("Accept")

    if (accept != null) {
      val acceptParts = Splitter.on(',').split(accept).asScala.toArray

      Sorting.quickSort(acceptParts)(AcceptOrdering)

      val seq = acceptParts.map { xs =>
        val part = Splitter.on(";q=").split(xs).asScala.head

        ContentType(part).getOrElse(new ContentType.All)
      }

      seq
    } else {
      Seq.empty[ContentType]
    }
  }



  private val _decoderResult = DecoderResult.SUCCESS
  override def decoderResult: DecoderResult = _decoderResult
  override def getDecoderResult: DecoderResult = _decoderResult
  override def setDecoderResult(decoderResult: DecoderResult): Unit = {}
  
  
  //HttpRequest implementation
  override def isRequest: Boolean = true
  def getMethod: HttpMethod = request.method()
  var http_version: HttpVersion = request.protocolVersion()
	override def getMethodName: String = request.method().toString
  //val decodedUri: String = URLDecoder.decode(request.uri(), "UTF-8")
	override def getUri: String = URLDecoder.decode(request.uri(), "UTF-8")

  lazy val headerMap: HeaderMap =
    new MapHeaderMap(mutable.Map(request.headers().entries().asScala.map(kv => kv.getKey -> Seq(kv.getValue)): _*))
  override def headers(): HttpHeaders = request.headers()

	override def getHeader(name: String): Option[String] = headerMap.get(name)
	override def remoteSocketAddress: InetSocketAddress = _request._remoteAddress
	private[server] var mapParams = {
    var params = immutable.Map[String, Seq[String]]()
    val q_ind = request.uri().indexOf("?")
    if (q_ind > -1) {
      request.uri().substring(q_ind + 1).split("&").foreach(params_pairs => {
        Logger.debug("Adding parameters: " + params_pairs)
        val splits = params_pairs.split("=")
        Logger.debug("Adding parameters: " + splits.toList)
        if (splits.length == 2) {
          val key = URLDecoder.decode(splits(0), "UTF-8")
          if (key.endsWith("[]")) {
            val theKey = key.substring(0, key.length-2)
            val pSeq: Seq[String] = params.getOrElse(theKey, Nil) :+ splits(1)
            params += theKey -> pSeq
          } else
            params += key -> Seq(URLDecoder.decode(splits(1), "UTF-8"))
        }
      })
    }
    _request.getPostParams.foreach(kv => params += kv._1 -> Seq(kv._2))
    new MapParamMap(underlying = params)
  }
	override def params: MapParamMap = mapParams

  private[server] var uploadingfiles: immutable.Map[String, FileParamHolder] = {
    _request.getFiles.map(kv => kv._1 -> {
      val fph = new StreamFileParamHolder(name = kv._1,
        mimeType = kv._2._1.mimeType,
        fileName = kv._2._1.fileName,
        dataIn = kv._2._1.data,
        size = kv._2._1.length)
      fph.complete = kv._2._2
      fph
    })
  }
  override def files: immutable.Map[String, FileParamHolder] = uploadingfiles

  override def isComplete: Boolean = _request.isComplete

	override def snapshot: HttpRequest = {
    new ClonedRequest(this)
  }

	//def params: ParamMap
  override val stateless_? : Boolean = !stateful
  override val session: Option[HttpSession] = if (stateful) {
    	Logger.info("init req session: " + cookies)
	    val sess_id = cookies.getValue("sessionid").getOrElse{Helpers.nextFuncName}
    	Logger.info("try get container session")
	    val s = SessionMaster.getContainerSession(sess_id)
	    Logger.info("has container session: " + s)
	    val session = s.openOr{
        Logger.info("reopen session")
	      val session = new HttpSession(sess_id)
        Logger.info("new session")
	      SessionMaster.putContainerSession(sess_id, session)
        Logger.info("put container session: " + sess_id)
	      session
	    }
    	Logger.info("req session init")
	    session.touchSession()

	    Some(session)
	  } else None
}

class ClonedRequest(request: Request) extends HttpRequest {
  //var multiParams:  Map[String, MultipartItem]  = request.multiParams.clone()
  var routeParams:  mutable.Map[String, String] = request.routeParams.clone()
  var error:        Option[Throwable]           = request.error

  def accepts: Seq[ContentType] = request.accepts

  //HttpRequest implementation
  private val httpMethod = request.getMethod
  def getMethod: HttpMethod = request.getMethod
	override def getMethodName: String = getMethod.toString
	private val _uri = request.getUri
	override def getUri: String = _uri
  //override lazy val headerMap: HeaderMap = request.headerMap.clone()
	override def getHeader(name: String): Option[String] = request.headerMap.get(name)
	private val _remoteSocketAddress = request.remoteSocketAddress
	override def remoteSocketAddress: InetSocketAddress = _remoteSocketAddress
	private val mapParams = MapParamMap(request.params)
	override def params: MapParamMap = mapParams

  private val uploadingfiles: immutable.Map[String, FileParamHolder] = request.files.map(fph => fph)
  override def files: immutable.Map[String, FileParamHolder] = uploadingfiles

	override def snapshot: Request = {
    throw new Exception("Shouldn't be snapshoted")
  }
	
	//def params: ParamMap
  private val stateful = request.stateful
  override val stateless_? : Boolean = !stateful
  private val sess_id: Option[String] = request.session.map(_.sessionId)
  override val session: Option[HttpSession] = if (stateful) {
	    sess_id.map(sess_id => {
	      val s = SessionMaster.getContainerSession(sess_id)
		    val session = s.openOr{
		      val session = new HttpSession(sess_id)
		      SessionMaster.putContainerSession(sess_id, session)
		      session
		    }
		    session.touchSession()
		    
		    session
	    })
	  } else None
}

object AcceptOrdering extends Ordering[String] {

  def getWeight(str: String): Double = {
    val parts = Splitter.on(';').split(str).asScala.toArray

    if (parts.length < 2) {
      1.0
    } else {
      try {
        Splitter.on("q=").split(parts(1)).asScala.toArray.last.toFloat
      } catch {
        case _:java.lang.NumberFormatException =>
          1.0
      }
    }
  }

  def compare(a: String, b: String): Int = {
    getWeight(b) compare getWeight(a)
  }
}


/**
 * Manages TheSessions.
 */
object SessionMaster {
  private var onGetUserData: Seq[(HttpSession, Seq[(String, JObject)]) => Unit] = Nil
  def addOnGetUserData(f: (HttpSession, Seq[(String, JObject)]) => Unit): Unit = {
    onGetUserData +:= f
  }

  private var onUserGetRequest: Seq[(String, String)] = Nil
  def addOnUserGetRequest(key: String, request: JObject): Unit = addOnUserGetRequest(Seq(key -> request))
  def addOnUserGetRequest(params: Seq[(String, JObject)]): Unit = {
    onUserGetRequest ++= params.map(kv => kv._1 -> kv._2.toString)
  }

  if (MindwebServer.getServerProperty("with_user").contains("true")) {
    LoginClient.init(ServerContext.flow_dispatcher)
    EdgeUser.onLogIn ::= ((e: AbstractEdgeUser, ip: String, userAgent: String) => e match {
      case e: EdgeUser =>
        val ud = Await.result(SessionClient.setUser(e.sessionId, ip, userAgent, e, onUserGetRequest), 15 seconds)
        Logger.debug("on set user done: " + ud)
        getSession(e.sessionId, None).foreach(_.httpSession.foreach(s => {
          onGetUserData.foreach(f => f(s, ud.data.flatMap(kv => Jckson.parse(kv.updateJson) match {
            case obj: JObject => Some(kv.key -> obj)
            case _ => None
          })))
        }))
        //Logger.debug("onLogIn: " + e.sessionId + " : " + R.session.map(_.uniqueId))
      case _ =>
    })
    EdgeUser.onLogOut ::= ((e: AbstractEdgeUser, ip: String, userAgent: String) => e match {
      case e: EdgeUser =>
        SessionClient.removeUser(e.sessionId, ip, userAgent, e)
      //Logger.debug("onLogIn: " + e.sessionId + " : " + R.session.map(_.uniqueId))
      case _ =>
    })
  }

  val cometPathPattern: PathPattern = SinatraPathPatternParser("/mw_comet/:sess_id/:page_id")
  def getTheSession(req: HttpRequest): TheSession = {
    val cometSessionId = req.params.get("sess_id").orElse {
      val route_params = cometPathPattern.apply(req.path.split('?').head)
      route_params.flatMap(_.get("sess_id").map(s => s.asInstanceOf[Seq[String]].head))
    }
    /*
    val route_params = cometPathPattern.apply(req.path.split('?').head)
    val cometSessionId = route_params.flatMap(_.get("sess_id").map(s => s.asInstanceOf[Seq[String]].head))
    * 
    */
    /*
    val wp = req.path.wholePath
    val CP = LiftRules.cometPath
    val cometSessionId = wp match {
      case CP :: _ :: session :: _ => Full(session)
      case _ => Empty
    }
    * 
    */

    val ret = getSession(req, cometSessionId) match {
      case Full(ret) => ret

      case FailureBox(_, _, _) => new TheSession(Helpers.nextFuncName, None) with StatelessSession

      case _ =>
        val ret = TheSession(req)
        addSession(ret, req, req.getHeader("User-Agent"), Some(getIpFromReq(req)))
        ret
    }

    if (MindwebServer.getServerProperty("with_user").contains("true")) {
      SessionClient.keepAliveSession(ret.uniqueId)
    }

    makeCometBreakoutDecision(ret, req)
    ret.touchSession()
    ret
  }
  
  
  import java.util.concurrent.ConcurrentHashMap
  private val rwl = new java.util.concurrent.locks.ReentrantReadWriteLock()
  
  private val killedSessions: scala.collection.concurrent.Map[String, Long] = new ConcurrentHashMap[String, Long]().asScala
  private val container_sessions: scala.collection.concurrent.Map[String, HttpSession] = new ConcurrentHashMap[String, HttpSession]().asScala
  
  
  def getContainerSession(id: String): Box[HttpSession] = {
    val dead = killedSessions.contains(id)

    if (dead) FailureBox("Dead session", Empty, Empty) else {
      /*
      Logger.debug("getContainerSession: " +
        rwl.getReadHoldCount + " : " +
        rwl.getReadLockCount + " : " +
        rwl.getWriteHoldCount)

       */
      //Logger.debug("try lock")
      rwl.readLock.lock()
      //Logger.debug("in a lock")
      val ret = container_sessions.get(id)
      rwl.readLock.unlock()
      //Logger.debug("out a lock")
      ret
    }
  }
  def putContainerSession(id: String, session: HttpSession): Unit = {
      /*
    Logger.debug("putContainerSession: " +
      rwl.getReadHoldCount + " : " +
      rwl.getReadLockCount + " : " +
      rwl.getWriteHoldCount)
       */
    //Logger.debug("try lock")
    rwl.readLock.lock()
    //Logger.debug("in a lock")
    container_sessions.put(id, session)
    rwl.readLock.unlock()
    //Logger.debug("out a lock")
  }
  
  private val nsessions: scala.collection.concurrent.Map[String, SessionInfo] = new ConcurrentHashMap[String, SessionInfo]().asScala

  //private object CheckAndPurge

  /**
   * If you have a rule other than <pre>Box !! req.request.remoteAddress</pre>
   * for calculating the remote address, change this function
   */
  @volatile var getIpFromReq: HttpRequest => String = req => req.remoteAddress.toString//.map(_.to)

  def getSession(req: HttpRequest, otherId: Option[String]): Box[TheSession] = {
    val dead = otherId.exists(killedSessions.contains(_))

    Logger.debug("get sessions: " + nsessions)

    if (dead) FailureBox("dead session", Empty, Empty) else {
	    otherId.flatMap(a => nsessions.get(a)) match {
	      case Some(session) => putSession(Full(session), Some(req))
	      // for stateless requests, vend a stateless session if none is found
	      case _ if req.stateless_? =>
	        putSession(
	          req.session.map(_.sessionId).flatMap(a => nsessions.get(a)),
            Some(req)
          ) or Full(new TheSession(Helpers.nextFuncName, None) with StatelessSession)
	      case _ => putSession({
	        //Logger.debug("getSession putSession: " + otherId + " : " + req.session.map(_.sessionId))
			      otherId.flatMap(a => nsessions.get(a)) orElse
			      req.session.map(_.sessionId).flatMap(id => nsessions.get(id))
			    }, Some(req))
	    }
    }
  }
  
  def getSession(id: String, otherId: Option[String]): Box[TheSession] = putSession {
    val dead = killedSessions.contains(id) || otherId.exists(killedSessions.contains)

    if (dead) FailureBox("Dead session", Empty, Empty) else {
      Logger.debug("getSession: " +
        rwl.getReadHoldCount + " : " +
        rwl.getReadLockCount + " : " +
        rwl.getWriteHoldCount)
      Logger.debug("try lock")
      rwl.readLock.lock()
      Logger.debug("in a lock")
      val ret = Box.option2Box(otherId).flatMap(a => nsessions.get(a)) or nsessions.get(id)
      rwl.readLock.unlock()
      Logger.debug("out a lock")

      ret
    }
  }
  
  /**
   * Adds a new session to SessionMaster
   */
  def addSession(theSession: TheSession, req: HttpRequest,
                 userAgent: Option[String], ipAddress: Option[String]) {
    putSession {
      Full(SessionInfo(theSession, userAgent, ipAddress, -1, 0L))
    }

    //Logger.info("addSession")

    R.init(req, theSession) {
      theSession.startSession()
      TheSession.afterSessionCreate.foreach(_(theSession, req))
    }

    theSession.httpSession.foreach(_.link(theSession))
  }

  def initSession(theSession: TheSession, userAgent: Option[String], ipAddress: Option[String]) = theSession.synchronized{
    if (!theSession.isInited && MindwebServer.getServerProperty("with_user").contains("true")) {
      Logger.debug("Reinit session")
      killedSessions.remove(theSession.uniqueId)
      val storedSessionDataFuture = SessionClient.registerSession(theSession.uniqueId,
        ipAddress.getOrElse(""),
        userAgent.getOrElse("")
      )

      val sessionUserFuture = SessionClient.getUser(theSession.uniqueId,
        onUserGetRequest)

      //TODO: Восстановить сессию
      Logger.debug("Try restore session")
      val storedSessionData = Await.result(storedSessionDataFuture, Timeout(10 seconds).duration)
      Logger.debug("Restored session data: " + storedSessionData)
      theSession.httpSession.foreach(s => {
        Logger.debug("Restoring session data")
        storedSessionData.updates.foreach(kv => kv.value.foreach(v => s.setAttribute(kv.key, v)))
        s._onStoreChange = Some((key, value, ip, userAgent) => {
          Logger.debug("Update session data")
          SessionClient.setSessionData(theSession.uniqueId, ip, userAgent, Seq((key, value)))
        })
        s._onPersistantAdd = Some((key, value, ip, userAgent, user) => {
          SessionClient.persist(theSession.uniqueId, user.flatMap(u =>
            Helpers.tryo{u.asInstanceOf[AbstractEdgeUser]}).orElse(EdgeUser.currentUser), Seq(key -> value),
            ip, userAgent).map(ps => {
            (ps.status, ps.message.getOrElse(""), ps.data.map(kv => {
              kv.key -> Jckson.parse(kv.updateJson)
            }))
          })(ServerContext.grpc_dispatcher)
        })
      })

      val storedSessionUser = Await.result(sessionUserFuture, Timeout(30 seconds).duration)
      Logger.debug("try restore session user: " + storedSessionUser)
      val theUser = if (storedSessionUser.info.sessionId == theSession.uniqueId &&
        (storedSessionUser.info.userId.nonEmpty || storedSessionUser.info.entityUserId.nonEmpty)) {
        Logger.debug("restore session user")
        val user = new EdgeUser(storedSessionUser.info.userId,
          storedSessionUser.info.entityUserId.map(e => EntityUserId(e.entityId, e.userId)), storedSessionUser.info.sessionId)
        theSession.restored += "edgeuser" -> user
        user
      } else None

      Logger.debug("Got USER OR SESSION DATA:")
      Logger.debug(storedSessionUser.data)

      theSession.httpSession.foreach(s => {
        onGetUserData.foreach(f => f(s, storedSessionUser.data.flatMap(k => Jckson.parse(k.updateJson) match {
          case o: JObject => Some(k.key -> o)
          case _ => None
        })))
      })
    }

    theSession.initDone()
  }

  /*
  def breakOutAllComet(): Unit = {
    Logger.debug("breakOutAllComet: " +
      rwl.getReadHoldCount + " : " +
      rwl.getReadLockCount + " : " +
      rwl.getWriteHoldCount)
    Logger.debug("try lock")
    rwl.writeLock.lock()
    Logger.debug("in a lock")
    nsessions.valuesIterator.foreach {
      _.session.breakOutComet()
    }
    rwl.writeLock.unlock()
    Logger.debug("out a lock")
  }
   */

  /**
   * Put an Actor in this list and the Actor will receive a message
   * every 10 seconds with the current list of sessions:
   * SessionWatcherInfo
   */
  //import akka.actor.ActorRef
  //@volatile var sessionWatchers: java.util.concurrent.ConcurrentLinkedQueue[ActorRef] = new java.util.concurrent.ConcurrentLinkedQueue()


  /**
   * Increments the count and last access time for the session
   */
  private def putSession(s: Box[SessionInfo], req: Option[HttpRequest] = None): Box[TheSession] = {
    Logger.debug("putSession: " +
      rwl.getReadHoldCount + " : " +
      rwl.getReadLockCount + " : " +
      rwl.getWriteHoldCount)
    Logger.debug("try lock")
    rwl.readLock.lock()
    Logger.debug("in a lock")
    //Logger.debug("putSession: " + s)
    val res = s.map {
      s =>
      nsessions.put(s.session.uniqueId, SessionInfo(s.session, s.userAgent, s.ipAddress, s.requestCnt + 1, System.currentTimeMillis()))
      s.session
    }

    rwl.readLock.unlock()
    Logger.debug("out a lock")

    res.foreach(s => {
      try {
        initSession(s, req.flatMap(_.getHeader("User-Agent")),
          req.map(getIpFromReq(_)))
      } catch {
        case e: Throwable =>
          e.printStackTrace()
      }
    })
    res
  }

  /**
   * Shut down all sessions
   */
  def shutDownAllSessions() {
    Logger.debug("shutDownAllSessions: " +
      rwl.getReadHoldCount + " : " +
      rwl.getReadLockCount + " : " +
      rwl.getWriteHoldCount)
    Logger.debug("try lock")
    rwl.writeLock().lock()
    Logger.debug("in a lock")
    nsessions.foreach {kv =>
      val sess = kv._2
      if (!sess.session.markedForShutDown_?) {
        sess.session.markedForShutDown_? = true
        removeSession(kv._1)
      }
    }

    /*
    while (true) {
      val s2 = lockRead(nsessions)
      if (s2.size == 0) return
      Thread.sleep(50)
    }
    * 
    */
    rwl.writeLock().unlock()
    Logger.debug("out a lock")
  }

  def isDead(sessionId: String): Boolean = killedSessions.contains(sessionId)
  
  def removeSession(sessionId: String): Unit = {
    Logger.debug("removeSession: " + sessionId + " : " +
      rwl.getReadHoldCount + " : " +
      rwl.getReadLockCount + " : " +
      rwl.getWriteHoldCount)
    Logger.debug("try lock !")
    rwl.writeLock().lock()
    Logger.debug("in a lock !")
    nsessions.get(sessionId).foreach {sess =>
      val s = sess.session
      killedSessions.put(s.uniqueId, System.currentTimeMillis())
      s.markedForShutDown_? = true
      try {
        s.doShutDown()
        try {
          s.httpSession.foreach(_.unlink(s))
        } catch {
          case _: Exception => // ignore... sometimes you can't do this and it's okay
        }
      } catch {
        case e: Exception => Logger.warn("Failure in remove session", e)
      }

      nsessions.remove(sessionId)

      //TODO: removeSession
      if (MindwebServer.getServerProperty("with_user").contains("true")) {
        SessionClient.removeSession(sessionId)
      }
    }
    rwl.writeLock().unlock()
    Logger.debug("out a lock !!")
  }

  ServerContext.system.scheduler.scheduleAtFixedRate(1 minutes, 1 minutes)(() => {
    //Logger.debug("scheduleAtFixedRate")
    /* remove dead sessions that are more than 45 minutes old */
    val now = System.currentTimeMillis() - 45 * 60 * 1000 // minutes

    val removeKeys: Iterable[String] = killedSessions.filter(_._2 < now).keys
    removeKeys.foreach(s => killedSessions.remove(s))


    val millis = System.currentTimeMillis()

    rwl.writeLock().lock()
    for ((id, info@SessionInfo(session, _, _, _, _)) <- nsessions) {
      if ((millis - session.getLastServicetime > session.getInactivityLength) || session.markedForTermination) {
        Logger.info(" Session " + id + " expired")
        if (!info.session.markedForShutDown_?) {
          info.session.markedForShutDown_? = true
          removeSession(info.session.uniqueId)
        }
      } else {
        session.doCometActorCleanup()
        session.cleanupUnseenFuncs()
      }
    }

    val removeSessKeys: Iterable[String] = container_sessions.filter(s => millis - s._2.lastAccessedTime > s._2.maxInactiveInterval).keys

    removeSessKeys.foreach(s => container_sessions.remove(s))
    rwl.writeLock().unlock()

    //sessionWatchers.foreach(_ ! SessionWatcherInfo(ses))
  })(ServerContext.task_dispatcher)

  
  /**
   * A function that takes appropriate action in breaking out of any
   * existing comet requests based on the request, browser type, etc.
   */
  def makeCometBreakoutDecision(session: TheSession, req: HttpRequest): Unit = {
    // get the open sessions to the host (this means that any DNS wildcarded
    // Comet requests will not be counted), as well as all invalid/expired
    // sessions
    
    /*
    val (which, invalid) = session.cometForHost(req.hostAndPath)

    // get the maximum requests given the browser type
    val max = maxConcurrentRequests.vend(req) - 2 // this request and any open comet requests

    // dump the oldest requests
    which.dropRight(max).foreach {
      case (actor, req) => actor ! BreakOut()
    }
    invalid.foreach {
      case (actor, req) => actor ! BreakOut()
    }
    * 
    */
  }
/*
  private[http] def sendMsg(in: Any): Unit =
    if (!Props.inGAE) this ! in
    else {
      lockWrite {
        tryo {
          if (reaction.isDefinedAt(in)) reaction.apply(in)
        }
      }
    }

  private def doPing() {
    if (!Props.inGAE) {
      try {
        Schedule.schedule(this, CheckAndPurge, 10 seconds)
      } catch {
        case e: Exception => Logger.error("Couldn't start SessionMaster ping", e)
      }
    }
  }

  doPing()
  * 
  */
}
