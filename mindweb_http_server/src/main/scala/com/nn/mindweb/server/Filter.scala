package com.nn.mindweb.server

import java.util.Date

import scala.concurrent.Future
import ServerContext._

/**
 * A Filter acts as a decorator/transformer of a service. It may apply
 * transformations to the input and output of that service:
 *
 *           (*  MyService  *)
 * [ReqIn -> (ReqOut -> RepIn) -> RepOut]
 *
 * For example, you may have a POJO service that takes Strings and
 * parses them as Ints.  If you want to expose this as a Network
 * Service via Thrift, it is nice to isolate the protocol handling
 * from the business rules. Hence you might have a Filter that
 * converts back and forth between Thrift structs. Again, your service
 * deals with POJOs:
 *
 * [ThriftIn -> (String  ->  Int) -> ThriftOut]
 *
 * Thus, a Filter[A, B, C, D] converts a Service[C, D] to a Service[A, B].
 * In other words, it converts a Service[ReqOut, RepIn] to a
 * Service[ReqIn, RepOut].
 *
 */
abstract class Filter[-ReqIn, +RepOut, +ReqOut, -RepIn]
  extends ((ReqIn, Service[ReqOut, RepIn]) => Future[RepOut])
{
  /**
   * This is the method to override/implement to create your own Filter.
   *
   * @param  request  the input request type
   * @param  service  a service that takes the output request type and the input response type
   *
   */
  def apply(request: ReqIn, service: Service[ReqOut, RepIn]): Future[RepOut]

  /**
   * Chains a series of filters together:
   *
   *    myModularService = handleExceptions.andThen(thrift2Pojo.andThen(parseString))
   *
   * '''Note:''' synchronously thrown exceptions in the underlying service are automatically
   * lifted into Future.exception.
   *
   * @param  next  another filter to follow after this one
   *
   */
  def andThen[Req2, Rep2](next: Filter[ReqOut, RepIn, Req2, Rep2]) =
    new Filter[ReqIn, RepOut, Req2, Rep2] {
      def apply(request: ReqIn, service: Service[Req2, Rep2]) = {
        Filter.this.apply(request, new Service[ReqOut, RepIn] {
          def apply(request: ReqOut): Future[RepIn] = next(request, service)
          override def close(deadline: Date) = service.close(deadline)
          override def isAvailable = service.isAvailable
        })
      }
    }

  /**
   * Terminates a filter chain in a service. For example,
   *
   *     myFilter.andThen(myService)
   *
   * @param  service  a service that takes the output request type and the input response type.
   *
   */
  def andThenS(service: Service[ReqOut, RepIn]) = new Service[ReqIn, RepOut] {
    def apply(request: ReqIn) = Filter.this.apply(request, Service.rescue(service))
    override def close(deadline: Date) = service.close(deadline)
    override def isAvailable = service.isAvailable
  }

  def andThen(f: ReqOut => Future[RepIn]): ReqIn => Future[RepOut] = {
    val service = Service.mk(f)
    (req) => Filter.this.apply(req, service)
  }

  def andThen(factory: ServiceFactory[ReqOut, RepIn]): ServiceFactory[ReqIn, RepOut] =
    new ServiceFactory[ReqIn, RepOut] {
      def apply(conn: ClientConnection) = factory(conn) map { Filter.this andThenS }
      def close(deadline: Date) = factory.close(deadline)
      override def isAvailable = factory.isAvailable
      override def toString = factory.toString
    }

  /**
   * Conditionally propagates requests down the filter chain. This may
   * useful if you are statically wiring together filter chains based
   * on a configuration file, for instance.
   *
   * @param  condAndFilter  a tuple of boolean and filter.
   *
   */
  def andThenIf[Req2 >: ReqOut, Rep2 <: RepIn](
    condAndFilter: (Boolean, Filter[ReqOut, RepIn, Req2, Rep2])) =
    condAndFilter match {
      case (true, filter) => andThen(filter)
      case (false, _)     => this
    }
}

abstract class SimpleFilter[Req, Rep] extends Filter[Req, Rep, Req, Rep]

object Filter {
  implicit def canStackFromSvc[Req, Rep]
    : CanStackFrom[Filter[Req, Rep, Req, Rep], Service[Req, Rep]] =
    new CanStackFrom[Filter[Req, Rep, Req, Rep], Service[Req, Rep]] {
      def toStackable(headRole: Stack.Role, filter: Filter[Req, Rep, Req, Rep]) =
        new Stack.Simple[Service[Req, Rep]](headRole) {
          val description = headRole.toString
          def make(params: Params, next: Service[Req, Rep]) = filter andThenS next
        }
    }

  implicit def canStackFromFac[Req, Rep]
    : CanStackFrom[Filter[Req, Rep, Req, Rep], ServiceFactory[Req, Rep]] =
    new CanStackFrom[Filter[Req, Rep, Req, Rep], ServiceFactory[Req, Rep]] {
      def toStackable(headRole: Stack.Role, filter: Filter[Req, Rep, Req, Rep]) =
        new Stack.Simple[ServiceFactory[Req, Rep]](headRole) {
          val description = headRole.toString
          def make(params: Params, next: ServiceFactory[Req, Rep]) = filter andThen next
        }
    }


  def identity[Req, Rep]: SimpleFilter[Req, Rep] = new SimpleFilter[Req, Rep] {
    override def andThen[Req2, Rep2](next: Filter[Req, Rep, Req2, Rep2]) = next
    override def andThenS(service: Service[Req, Rep]) = service
    override def andThen(factory: ServiceFactory[Req, Rep]) = factory

    def apply(request: Req, service: Service[Req, Rep]) = service(request)
  }

  def mk[ReqIn, RepOut, ReqOut, RepIn](
    f: (ReqIn, ReqOut => Future[RepIn]) => Future[RepOut]
  ): Filter[ReqIn, RepOut, ReqOut, RepIn] = new Filter[ReqIn, RepOut, ReqOut, RepIn] {
    def apply(request: ReqIn, service: Service[ReqOut, RepIn]) = f(request, service)
  }
}