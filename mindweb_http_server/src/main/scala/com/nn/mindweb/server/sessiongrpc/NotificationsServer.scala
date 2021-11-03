package com.nn.mindweb.server.sessiongrpc

import java.util.concurrent.ConcurrentHashMap

import com.aklabs.sessions.session._
import com.nn.http.{CometUpdateJValStr, TheSession}
import com.nn.mindweb.server.dataadapter.StructuredFormDataAdapter
import com.nn.mindweb.server.{ServerContext, SessionMaster}
import io.grpc.{Server, ServerBuilder}
import net.aklabs.helpers.JsonHelpers.{JValue, Jckson}
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._
import scala.collection.concurrent
import scala.concurrent.{ExecutionContext, Future}

object NotificationsServer {
  var server: NotificationsServer = _
  def start(): Unit = {
    if (server != null) return
    server = new NotificationsServer(ServerContext.grpc_dispatcher)
    server.start()
    server.blockUntilShutdown()
  }
  def stop(): Unit = if (server != null) server.stop()

  private[sessiongrpc] val notificationListeners: concurrent.Map[String, (String, Boolean, Boolean) => Unit] =
    new ConcurrentHashMap[String, (String, Boolean, Boolean) => Unit]().asScala

  //(msg, done, fail)
  def addListener(operationId: String, listener: (String, Boolean, Boolean) => Unit): Unit = {
    notificationListeners += operationId -> listener
  }

  private[sessiongrpc] var notifyHooks: List[(TheSession, JValue) => Unit] = Nil

  def addNotifyHook(hook: (TheSession, JValue) => Unit): Unit = notifyHooks ::= hook
}

class NotificationsServer(executionContext: ExecutionContext) { self =>
  private[this] var server: Server = _
  private val port = 50060

  private def start(): Unit = {
    server = ServerBuilder.forPort(port)
      .addService(EdgeGrpc.bindService(new EdgeImpl, executionContext))
      .build.start

    Logger.info("Сервер сессий запущен " + port)
    sys.addShutdownHook {
      System.err.println("*** shutting down gRPC server since JVM is shutting down")
      self.stop()
      System.err.println("*** server shut down")
    }
  }

  private def stop(): Unit = {
    if (server != null) {
      server.shutdown()
    }
  }

  private def blockUntilShutdown(): Unit = {
    if (server != null) {
      server.awaitTermination()
    }
  }



  private class EdgeImpl extends EdgeGrpc.Edge {
    override def notify(sessionAddress: EdgeSessionAddress): Future[Exists] = {
      Logger.debug("Notfication send to frontend: " + sessionAddress)

      Future.successful{
        val sessionExists = SessionMaster.getSession(sessionAddress.sessionId, None).map(sess => {
          Logger.debug("Got session")

          if (NotificationsServer.notifyHooks.nonEmpty) try {
            val jv = Jckson.parse(sessionAddress.jv)
            NotificationsServer.notifyHooks.foreach(hook => {
              hook(sess, jv)
            })
          } catch {
            case e: Throwable =>
              e.printStackTrace()
          }

          sess.getAsyncComponent(sessionAddress.renderVersion).foreach(comet_actor => {
            Logger.debug("Found actor")
            comet_actor ! CometUpdateJValStr("notify", sessionAddress.jv)
          })
        }).isDefined

        Exists(sessionExists)
      }
    }

    override def notifyWorker(request: EdgeWorkerMessage): Future[Exists] = {
      Logger.debug("Notify worker: " + request)
      Future.successful{
        val processingFuncExisis = NotificationsServer.notificationListeners.get(request.frontOperationId).map(f => {
          f(request.jv, request.done, false)
          if (request.done)
            NotificationsServer.notificationListeners -= request.frontOperationId
          true
        }).getOrElse{
          StructuredFormDataAdapter.updateFrontOp(request.frontOperationId, request.jv, request.done)
        }
        Exists(processingFuncExisis)
      }
    }

    override def failWorker(request: EdgeWorkerFailure): Future[Exists] = {
      Logger.debug("Fail worker: " + request)
      Future.successful{
        val processingFuncExisis = NotificationsServer.notificationListeners.get(request.frontOperationId).map(f => {
          f(request.msg, false, true)
          NotificationsServer.notificationListeners -= request.frontOperationId
          true
        }).getOrElse{
          StructuredFormDataAdapter.failFrontOp(request.frontOperationId, request.msg)
        }
        Exists(processingFuncExisis)
      }
    }
  }
}