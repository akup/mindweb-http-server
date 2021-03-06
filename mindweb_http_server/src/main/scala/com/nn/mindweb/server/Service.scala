package com.nn.mindweb.server

import java.net.SocketAddress
import java.util.Date

import scala.concurrent.Future
import scala.concurrent.duration.Duration

import net.aklabs.helpers.TimeHelpers._
import ServerContext._

object Service {
  /**
   * Wrap an underlying service such that any synchronously thrown exceptions are lifted into
   * Future.exception
   */
  def rescue[Req, Rep](service: Service[Req, Rep]): ServiceProxy[Req, Rep] = new ServiceProxy[Req, Rep](service) {
    override def apply(request: Req): Future[Rep] = {
      try {
        service(request)
      } catch {
        case e: Throwable => Future{throw e}
      }
    }
  }

  def mk[Req, Rep](f: Req => Future[Rep]): Service[Req, Rep] = (req: Req) => f(req)
}

/**
 * A Service is an asynchronous function from Request to Future[Response]. It is the
 * basic unit of an RPC interface.
 *
 * '''Note:''' this is an abstract class (vs. a trait) to maintain java
 * compatibility, as it has implementation as well as interface.
 */
abstract class Service[-Req, +Rep] extends (Req => Future[Rep]) with Closable {
  def map[Req1](f: Req1 => Req): Service[Req1, Rep] = new Service[Req1, Rep] {
    def apply(req1: Req1): Future[Rep] = Service.this.apply(f(req1))
    override def close(deadline: Date): Future[Unit] = Service.this.close(deadline)
  }

  /**
   * This is the method to override/implement to create your own Service.
   */
  def apply(request: Req): Future[Rep]

  def close(deadline: Date): Future[Unit] = Future{}

  /**
   * Determines whether this service is available (can accept requests
   * with a reasonable likelihood of success).
   */
  def isAvailable: Boolean = true
}

/**
 * Information about a client, passed to a Service factory for each new
 * connection.
 */
trait ClientConnection extends Closable {
  /**
   * Host/port of the client. This is only available after `Service#connected`
   * has been signalled.
   */
  def remoteAddress: SocketAddress

  /**
   * Host/port of the local side of a client connection. This is only
   * available after `Service#connected` has been signalled.
   */
  def localAddress: SocketAddress
}

object ClientConnection {
  val nil: ClientConnection = new ClientConnection {
    private[this] val unconnected =
      new SocketAddress { override def toString = "unconnected" }
    def remoteAddress: SocketAddress = unconnected
    def localAddress: SocketAddress = unconnected
    def close(deadline: Date): Future[Unit] = Future{}
  }
}

/**
 * A simple proxy Service that forwards all calls to another Service.
 * This is useful if you want to wrap-but-modify an existing service.
 */
abstract class ServiceProxy[-Req, +Rep](val self: Service[Req, Rep])
  extends Service[Req, Rep] with Proxy
{
  def apply(request: Req): Future[Rep] = self(request)
  override def close(deadline: Date): Future[Unit] = self.close(deadline)
  override def isAvailable: Boolean = self.isAvailable
  override def toString: String = self.toString
}

abstract class ServiceFactory[-Req, +Rep]
  extends (ClientConnection => Future[Service[Req, Rep]])
  with Closable
{ self =>

  /**
   * Reserve the use of a given service instance. This pins the
   * underlying channel and the returned service has exclusive use of
   * its underlying connection. To relinquish the use of the reserved
   * Service, the user must call Service.close().
   */
  def apply(conn: ClientConnection): Future[Service[Req, Rep]]
  final def apply(): Future[Service[Req, Rep]] = this(ClientConnection.nil)

  /**
   * Apply `f` on created services, returning the resulting Future in their
   * stead. This is useful for implementing common factory wrappers that
   * only need to modify or operate on the underlying service.
   */
  def flatMap[Req1, Rep1](f: Service[Req, Rep] => Future[Service[Req1, Rep1]]): ServiceFactory[Req1, Rep1] =
    new ServiceFactory[Req1, Rep1] {
      override def apply(conn: ClientConnection): Future[Service[Req1, Rep1]] =
        self.apply(conn).flatMap{ service =>
          val futureService = f(service)
          futureService.failed.foreach(_ => service.close())
          futureService
        }
      override def close(deadline: Date): Future[Unit] = self.close(deadline)
      override def isAvailable: Boolean = self.isAvailable
      override def toString(): String = self.toString()
    }

  /**
   * Map created services. Useful for implementing common
   * styles of factory wrappers.
   */
  def map[Req1, Rep1](f: Service[Req, Rep] => Service[Req1, Rep1]): ServiceFactory[Req1, Rep1] =
    flatMap { s => Future{f(s)} }

  /**
   * Make a service that after dispatching a request on that service,
   * releases the service.
   */
  final def toService: Service[Req, Rep] = new FactoryToService(this)

  def isAvailable: Boolean = true
}

object ServiceFactory {
  def const[Req, Rep](service: Service[Req, Rep]): ServiceFactory[Req, Rep] = new ServiceFactory[Req, Rep] {
      private[this] val noRelease = Future{new ServiceProxy[Req, Rep](service) {
       // close() is meaningless on connectionless services.
       override def close(deadline: Date): Future[Unit] = Future{}
     }}

      def apply(conn: ClientConnection): Future[ServiceProxy[Req, Rep]] = noRelease
      def close(deadline: Date): Future[Unit] = Future{}
    }

  def apply[Req, Rep](f: () => Future[Service[Req, Rep]]): ServiceFactory[Req, Rep] =
    new ServiceFactory[Req, Rep] {
      def apply(_conn: ClientConnection): Future[Service[Req, Rep]] = f()
      def close(deadline: Date): Future[Unit] = Future{}
    }
}

trait ProxyServiceFactory[-Req, +Rep] extends ServiceFactory[Req, Rep] with Proxy {
  def self: ServiceFactory[Req, Rep]
  def apply(conn: ClientConnection): Future[Service[Req, Rep]] = self(conn)
  def close(deadline: Date): Future[Unit] = self.close(deadline)
  override def isAvailable: Boolean = self.isAvailable
}
/**
 * A simple proxy ServiceFactory that forwards all calls to another
 * ServiceFactory.  This is is useful if you to wrap-but-modify an
 * existing service factory.
 */
abstract class ServiceFactoryProxy[-Req, +Rep](_self: ServiceFactory[Req, Rep])
  extends ProxyServiceFactory[Req, Rep] {
  def self: ServiceFactory[Req, Rep] = _self
}

class FactoryToService[Req, Rep](factory: ServiceFactory[Req, Rep])
  extends Service[Req, Rep]
{
  def apply(request: Req): Future[Rep] =
    factory() flatMap { service =>
      val response = service(request)
      service(request) onComplete (_ => service.close())
      response
    }

  override def close(deadline: Date): Future[Unit] = factory.close(deadline)
  override def isAvailable: Boolean = factory.isAvailable
}

/**
 * A ServiceFactoryWrapper adds behavior to an underlying ServiceFactory.
 */
trait ServiceFactoryWrapper {
  def andThen[Req, Rep](factory: ServiceFactory[Req, Rep]): ServiceFactory[Req, Rep]
}

object ServiceFactoryWrapper {
  val identity: ServiceFactoryWrapper = new ServiceFactoryWrapper {
    def andThen[Req, Rep](factory: ServiceFactory[Req, Rep]): ServiceFactory[Req, Rep] = factory
  }
}








trait Closable { self =>

  /**
   * Close the resource. The returned Future is completed when
   * the resource has been fully relinquished.
   */
  final def close(): Future[Unit] = close(new Date(0))

  /**
   * Close the resource with the given deadline. This deadline is advisory,
   * giving the callee some leeway, for example to drain clients or finish
   * up other tasks.
   */
  def close(deadline: Date): Future[Unit]

  /**
   * Close the resource with the given timeout. This timeout is advisory,
   * giving the callee some leeway, for example to drain clients or finish
   * up other tasks.
   */
  def close(after: Duration): Future[Unit] =
    close(new Date(((System.nanoTime nanoseconds) + after).toMillis))
}