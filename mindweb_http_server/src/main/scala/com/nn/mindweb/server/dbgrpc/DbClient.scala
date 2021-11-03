package com.nn.mindweb.server.dbgrpc

import java.net.{InetSocketAddress, URI}
import java.util
import java.util.UUID
import java.util.concurrent.TimeUnit

import com.aklabs.login.EdgeUser
import com.nn.mindweb.server.MindwebServer
import io.grpc.NameResolver.ResolutionResult
import io.grpc.{Attributes, EquivalentAddressGroup, ManagedChannel, ManagedChannelBuilder, NameResolver}
import net.aklabs.Props
import net.aklabs.db.dbapi.{DBData, DBNodeRequest, DBOperationStatus, DBRequest, DBSave, DBServiceGrpc, HiddenPickDescriptor, IndexNames, LazyDescriptor, LazyLoad, MergeGroupedRule, RecordNames, SaveEl, SaveStatus, UserId, VarCacheKey, VarEl}
import net.aklabs.helpers.JsonHelpers.JValue
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.concurrent.Future

object DbClient {
  val addresses: util.List[EquivalentAddressGroup] = Props.get("db.hosts").get.split(",").toList.flatMap(a => {
    val splits = a.split(":")
    if (splits.length == 2)
      Some(new EquivalentAddressGroup(new InetSocketAddress(splits(0), splits(1).toInt)))
    else None
  }).asJava

  {
    if (addresses.isEmpty) throw new Exception("Нет адресов для DbService")
    Logger.debug("Количество адресов сервиса баз данных: " + addresses.size())
  }

  private val channel: ManagedChannel = ManagedChannelBuilder.forTarget("db_client")
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
  private val asyncStub: DBServiceGrpc.DBServiceStub = DBServiceGrpc.stub(channel)

  def get(requests: Seq[DBNodeRequest], isAjax: Option[Boolean],
          forceUser: Option[UserId] = None): Future[DBData] = {
    if (MindwebServer.getServerProperty("with_user").contains("true") && requests.nonEmpty) {
      Logger.debug("Try get db data")
      Logger.debug("Current user: " + EdgeUser.currentUser)
      val user = forceUser.orElse(EdgeUser.currentUser.map(u => {
        UserId(u.getUniqueId, u.getEntityId.map(eu => "%s|%s".format(eu.entity, eu.userId)))
      })).getOrElse(UserId(None, None))

      Logger.debug("Got user for db request: " + user)

      asyncStub.get(DBRequest(user, requests, isAjax))
    } else Future{
      DBData(DBOperationStatus.empty)
    }(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
  }

  def save(sequentialOpKey: String, operationId: String, requestName: String,
           saveRecords: Seq[SaveEl], partialPath: Option[String], mergeRules: Seq[MergeGroupedRule],
           forceUser: Option[UserId] = None,
           keepUniques: Seq[String] = Nil): Future[SaveStatus] = {
    Logger.debug("Call save " + requestName)
    Logger.debug("Current user: " + EdgeUser.currentUser)
    val user = forceUser.orElse(EdgeUser.currentUser.map(u => {
      UserId(u.getUniqueId, u.getEntityId.map(eu => "%s|%s".format(eu.entity, eu.userId)))
    })).getOrElse(UserId(None, None))

    Logger.debug("Call save (user) " + user)

    asyncStub.save(DBSave(sequentialOpKey, user, requestName, saveRecords, Some(operationId), partialPath = partialPath,
      mergeRules = mergeRules,
      keepUniques = keepUniques))
  }

  def getSequentialOperationId(operationIdPattern: Option[String], requestName: String,
                                       recordNames: List[String] = Nil, uuids: List[(String, String)] = Nil): String = {
    operationIdPattern.map(pattern => {
      var opId = pattern.replaceAll("""\{requestName\}""", requestName)
      EdgeUser.currentUser.foreach(u => {
        val userId = u.getEntityId.map(e => "e%s_%s".format(e.entity, e.userId)).getOrElse(u.getUniqueId.get)
        opId = opId.replaceAll("""\{userId\}""", userId)
      })
      var uuidsNumerated: Map[Int, String] = Map.empty
      var i = 0
      recordNames.foreach(rn => {
        uuidsNumerated += i -> uuids.find(_._1 == rn).get._2
        i += 1
      })
      def getUuid(id: Int): String = {
        val uuid = uuidsNumerated.getOrElse(id, "")
        if (uuid == "new") UUID.randomUUID().toString else uuid
      }
      opId = opId.replaceAll("""\{uuid\}""", getUuid(0))
      opId = """\{uuid(\d+)\}""".r.replaceAllIn(opId, m => {
        Logger.debug("Replace group: " + m.group(1))
        getUuid(m.group(1).toInt)
      })

      opId
    }).getOrElse{
      uuids.sortWith((a, b) => a._1.compareTo(b._1) < 0).map(x =>
        "%s=%s".format(x._1, if (x._2 == "new") UUID.randomUUID().toString else x._2)
      ).mkString("/")
    }
  }

  def getIndex(recordName: String): Future[Option[String]] = {
    getIndex(Seq(recordName)).map(_.get(recordName))(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
  }
  def getIndex(recordNames: Seq[String]): Future[Map[String, String]] = {
    asyncStub.getIndexName(RecordNames(recordNames)).map(index => {
      index.indecies.map(i => i.recordName -> i.indexName).toMap
    })(com.nn.mindweb.server.ServerContext.grpc_dispatcher)
  }

  def lazyLoad(requestName: String, uuid: String, reqNum: Int, descriptor: Int,
               vars: Map[String, (JValue, (Int, String))]): Future[LazyLoad] = {
    val user = EdgeUser.currentUser.map(u => {
      UserId(u.getUniqueId, u.getEntityId.map(eu => "%s|%s".format(eu.entity, eu.userId)))
    }).getOrElse(UserId(None, None))
    asyncStub.lazyLoad(LazyDescriptor(user, requestName, reqNum, descriptor, uuid, vars.toSeq.map(v => {
      VarEl(v._2._1.toString, v._1, VarCacheKey(v._2._2._1, v._2._2._2))
    })))
  }
  def hiddenPick(requestName: String, search: Option[String], limit: Option[Int], reqNum: Int, descriptor: Int,
                 vars: Map[String, (JValue, (Int, String))]): Future[LazyLoad] = {
    val user = EdgeUser.currentUser.map(u => {
      UserId(u.getUniqueId, u.getEntityId.map(eu => "%s|%s".format(eu.entity, eu.userId)))
    }).getOrElse(UserId(None, None))
    asyncStub.hiddenPick(HiddenPickDescriptor(user, requestName, reqNum, descriptor, limit, search, vars.toSeq.map(v => {
      VarEl(v._2._1.toString, v._1, VarCacheKey(v._2._2._1, v._2._2._2))
    })))
  }
}
