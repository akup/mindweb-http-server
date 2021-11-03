package com.nn.mindweb.server.sessiongrpc

import java.net.{InetSocketAddress, URI}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import java.{lang, util}

import akka.dispatch.MessageDispatcher
import com.aklabs.login.EdgeUser
import com.aklabs.sessions.session.{Empty, KeyValue, KeyedJson, PersistSt, PersistStatus, SessionData, SessionId, SessionIdWithRequest, SessionInfo, SessionsGrpc, UserData, UserDataResponse, UserRequest, UserSessionInfo, UserSessionInfoWithData, UserStoredData}
import com.aklabs.user.AbstractEdgeUser
import com.nn.http.{HttpRequest, R}
import com.nn.mindweb.server.ServerContext
import io.grpc.NameResolver.ResolutionResult
import io.grpc.{Attributes, EquivalentAddressGroup, ManagedChannel, ManagedChannelBuilder, NameResolver}
import net.aklabs.Props
import net.aklabs.helpers.JsonHelpers.JObject
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object SessionClient {
  val addresses: util.List[EquivalentAddressGroup] = Props.get("sessions.hosts").get.split(",").toList.flatMap(a => {
    val splits = a.split(":")
    if (splits.length == 2)
      Some(new EquivalentAddressGroup(new InetSocketAddress(splits(0), splits(1).toInt)))
    else None
  }).asJava

  {
    if (addresses.isEmpty) throw new Exception("Нет адресов для SessionsService")
    Logger.debug("Количество адресов сервиса сессий: " + addresses.size())

    Logger.debug(addresses)
  }

  private val channel: ManagedChannel = ManagedChannelBuilder.forTarget("session_client")
    .nameResolverFactory(new NameResolver.Factory() {
      override def getDefaultScheme: String = "multiaddress"
      override def newNameResolver(uri: URI, args: NameResolver.Args): NameResolver = new NameResolver() {
        override def getServiceAuthority: String = "aklabs"
        override def start(listener: NameResolver.Listener2): Unit = {
          listener.onResult(ResolutionResult.newBuilder.setAddresses(addresses).setAttributes(Attributes.EMPTY).build)
        }

        def shutdown(): Unit = {}
      }
    }).defaultLoadBalancingPolicy("round_robin")
    .idleTimeout(2, TimeUnit.MINUTES)
    .keepAliveTime(10, TimeUnit.SECONDS)
    .keepAliveTimeout(2, TimeUnit.SECONDS)
    .usePlaintext()
    .build
  private val asyncStub: SessionsGrpc.SessionsStub = SessionsGrpc.stub(channel)


  val registerSessionMap: ConcurrentHashMap[String, (String, String)] = new ConcurrentHashMap[String, (String, String)]()
  val removeSessionSet: ConcurrentHashMap.KeySetView[String, lang.Boolean] = ConcurrentHashMap.newKeySet[String]()

  def registerSession(sessionId: String, ip: String, userAgent: String): Future[SessionData] = {
    //Logger.debug("retry registerSession: " + sessionId)
    val f = asyncStub.register(SessionInfo(sessionId, ip, userAgent))
    f.onComplete{
      case Failure(_) =>
        //Logger.debug("Add registerSessionMap")
        registerSessionMap.put(sessionId, ip -> userAgent)
      case Success(_) =>
        registerSessionMap.remove(sessionId)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
    f
  }
  def removeSession(sessionId: String): Future[Empty] = {
    //Logger.debug("retry removeSession: " + sessionId)
    val f = asyncStub.remove(SessionId(sessionId))
    f.onComplete{
      case Failure(_) =>
        removeSessionSet.add(sessionId)
      case Success(_) =>
        removeSessionSet.remove(sessionId)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
    f
  }
  def keepAliveSession(sessionId: String): Future[Empty] = {
    asyncStub.alive(SessionId(sessionId)).map(alive => {
      Empty()
    })(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
  }
  def setSessionData(sessionId: String, ip: String, userAgent: String,
                     data: Seq[(String, Option[String])]): Future[Empty] = {
    val f = asyncStub.data(SessionData(sessionId, ip, userAgent, data.map(kv => KeyValue(kv._1, kv._2))))
    f.onComplete{
      case Failure(_) =>
        //removeSessionSet.add(sessionId)
      case Success(_) =>
        //removeSessionSet.remove(sessionId)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
    f
  }
  def setUser(sessionId: String, ip: String, userAgent: String, user: EdgeUser,
              requests: Seq[(String, String)]): Future[UserStoredData] = {
    Logger.debug("set session user: " + sessionId + " : " + user.getUniqueId + " , " + user.getEntityId)
    val f = asyncStub.setUser(UserSessionInfo(sessionId, ip, userAgent, user.getUniqueId,
      user.getEntityId.map(eid => {
        com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
      }), requests.map(r => KeyedJson(r._1, r._2))))
    f.onComplete{
      case Failure(_) =>
      //removeSessionSet.add(sessionId)
      case Success(_) =>
      //removeSessionSet.remove(sessionId)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
    f
  }
  def removeUser(sessionId: String, ip: String, userAgent: String, user: EdgeUser): Future[Empty] = {
    Logger.debug("remove session user: " + sessionId + " : " + user.getUniqueId + " , " + user.getEntityId)
    val f = asyncStub.removeUser(UserSessionInfo(sessionId, ip, userAgent, user.getUniqueId,
      user.getEntityId.map(eid => {
        com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
      })))
    f.onComplete{
      case Failure(_) =>
      //removeSessionSet.add(sessionId)
      case Success(_) =>
      //removeSessionSet.remove(sessionId)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
    f
  }
  def getUser(sessionId: String, requests: Seq[(String, String)]): Future[UserSessionInfoWithData] = {
    Logger.debug("get session user: " + sessionId)
    asyncStub.getUser(SessionIdWithRequest(sessionId, requests.map(r => KeyedJson(r._1, r._2))))
  }

  def persist(sessionId: String, user: Option[AbstractEdgeUser],
              data: Seq[(String, JObject)],
              ip: Option[String], userAgent: Option[String]): Future[PersistStatus] = {
    implicit val dispatcher: ExecutionContext = ServerContext.grpc_dispatcher
    val _ip = ip.orElse(R.request.map(_.remoteAddress.toString))
    val _userAgent = userAgent.orElse(R.request.map(_.getHeader("User-Agent").toString))
    val theSession = R.httpSession
    asyncStub.persist(UserData(
      user.flatMap(_.getUniqueId),
      user.flatMap(_.getEntityId.map(eid => {
        com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
      })), sessionId,

      _ip, _userAgent,

      data.map(d => KeyedJson(d._1, d._2.toString))
    )).flatMap(ps => {
      if (PersistSt.invalid_user.id == ps.status) {
        Logger.debug("Восстанавливаем пользователя")
        theSession match {
          case Some(session) if session.sessionId == sessionId && _ip.nonEmpty && _userAgent.nonEmpty =>
            Logger.debug("Восстанавливаем пользователя, есть сессия")
            R.init(HttpRequest.nil(), session.linkedSuperSession.get) {
              EdgeUser.currentUser match {
                case Some(edgeUser) =>
                  Logger.debug("Восстанавливаем пользователя, есть пользователь")
                  setUser(sessionId, _ip.get, _userAgent.get, edgeUser.asInstanceOf[EdgeUser], Nil).flatMap(_ => {
                    val globalAttrs = session.getGlobalAttributes()
                    val sessionDataSetDone = if (globalAttrs.nonEmpty) setSessionData(sessionId, _ip.get, _userAgent.get, globalAttrs)
                    else Future{Empty}

                    Logger.debug("On resave session global data")
                    sessionDataSetDone.flatMap(_ => {
                      Logger.debug("Done resave session global data")
                      asyncStub.persist(UserData(
                        user.flatMap(_.getUniqueId),
                        user.flatMap(_.getEntityId.map(eid => {
                          com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
                        })), sessionId,

                        _ip, _userAgent,

                        data.map(d => KeyedJson(d._1, d._2.toString))
                      ))
                    })
                  })
                case _ =>
                  Logger.debug("Восстанавливаем пользователя, нет пользователя")
                  val globalAttrs = session.getGlobalAttributes()
                  if (globalAttrs.nonEmpty)
                    setSessionData(sessionId, _ip.get, _userAgent.get, globalAttrs)
                  Future{ps}
              }
            }
          case _ =>
            Logger.debug("Восстанавливаем пользователя, нет сессии")
            Future{ps}
        }
      } else Future{ps}
    })
  }

  def getPersistantData(sessionId: String, user: Option[AbstractEdgeUser],
                        requests: Seq[(String, JObject)],
                        ip: Option[String], userAgent: Option[String]): Future[UserDataResponse] = {
    implicit val dispatcher: ExecutionContext = ServerContext.grpc_dispatcher
    val _ip = ip.orElse(R.request.map(_.remoteAddress.toString))
    val _userAgent = userAgent.orElse(R.request.map(_.getHeader("User-Agent").toString))
    val theSession = R.httpSession
    asyncStub.getPersistantData(UserRequest(
      user.flatMap(_.getUniqueId),
      user.flatMap(_.getEntityId.map(eid => {
        com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
      })), sessionId,

      _ip, _userAgent,

      requests.map(r => KeyedJson(r._1, r._2.toString))
    )).flatMap(udr => {
      if (PersistSt.invalid_user.id == udr.status) {
        Logger.debug("Восстанавливаем пользователя")
        theSession match {
          case Some(session) if session.sessionId == sessionId && _ip.nonEmpty && _userAgent.nonEmpty =>
            Logger.debug("Восстанавливаем пользователя, есть сессия")
            R.init(HttpRequest.nil(), session.linkedSuperSession.get) {
              EdgeUser.currentUser match {
                case Some(edgeUser) =>
                  Logger.debug("Восстанавливаем пользователя, есть пользователь")
                  setUser(sessionId, _ip.get, _userAgent.get, edgeUser.asInstanceOf[EdgeUser], Nil).flatMap(_ => {
                    val globalAttrs = session.getGlobalAttributes()
                    val sessionDataSetDone = if (globalAttrs.nonEmpty) setSessionData(sessionId, _ip.get, _userAgent.get, globalAttrs)
                    else Future{Empty}

                    Logger.debug("On resave session global data")
                    sessionDataSetDone.flatMap(_ => {
                      Logger.debug("Done resave session global data")
                      asyncStub.getPersistantData(UserRequest(
                        user.flatMap(_.getUniqueId),
                        user.flatMap(_.getEntityId.map(eid => {
                          com.aklabs.sessions.session.EntityUserId(eid.entity, eid.userId)
                        })), sessionId,

                        _ip, _userAgent,

                        requests.map(r => KeyedJson(r._1, r._2.toString))
                      ))
                    })
                  })
                case _ =>
                  Logger.debug("Восстанавливаем пользователя, нет пользователя")
                  val globalAttrs = session.getGlobalAttributes()
                  if (globalAttrs.nonEmpty)
                    setSessionData(sessionId, _ip.get, _userAgent.get, globalAttrs)
                  Future{udr}
              }
            }
          case _ =>
            Logger.debug("Восстанавливаем пользователя, нет сессии")
            Future{udr}
        }
      } else Future{udr}
    })
  }


  ServerContext.system.scheduler.scheduleAtFixedRate(1 minute, 1 minute)(() => {
    val it = registerSessionMap.entrySet().iterator()
    while (it.hasNext) {
      val e = it.next()
      registerSession(e.getKey, e.getValue._1, e.getValue._2)
    }
    val itS = removeSessionSet.iterator()
    while (itS.hasNext) {
      removeSession(itS.next())
    }
  })(ServerContext.task_dispatcher)
}
