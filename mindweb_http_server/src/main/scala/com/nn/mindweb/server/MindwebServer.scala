package com.nn.mindweb.server

import akka.actor.ActorSystem
import akka.dispatch.MessageDispatcher
import com.nn.mindweb.server.ServerContext._
import com.nn.mindweb.server.ajaxsite._
import com.nn.mindweb.server.frontasync._
import com.nn.mindweb.server.messages._
import com.nn.mindweb.server.netty._
import com.nn.mindweb.server.sessiongrpc._
import net.aklabs.DefaultSystemContext
import net.aklabs.helpers.Helpers
import net.aklabs.http.CometContext
import net.aklabs.mindweb.server.ClosableWrapper
import net.aklabs.regbox.{BootstrapDI, Bootstrapper, PostWriter}
import org.pmw.tinylog.{Configurator, Logger}

import java.io.{File, InputStream}
import java.util.Properties
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}
import scala.language.postfixOps

object ServerContext {
  val system: ActorSystem = ActorSystem("ServerSystem")
  implicit val flow_dispatcher: MessageDispatcher = system.dispatchers.lookup("akka.actor.flow-dispatcher")
	val grpc_dispatcher: MessageDispatcher = system.dispatchers.lookup("akka.actor.grpc-dispatcher")
	val task_dispatcher: MessageDispatcher = system.dispatchers.lookup("akka.actor.task-dispatcher")
}
/**
 * Сервер, построенный на базе Finagle по образу
 * и подобию Finatra.
 *
 */
object MindwebServer {
	//3Mb - default
	val maxRequest: Long = getServerProperty("max_request_size").map(_.toLong).getOrElse(3072000L)
	val disableMultipartFiles: Boolean = getServerProperty("no_multipart_files").contains("ture")

	private val rwl = new java.util.concurrent.locks.ReentrantReadWriteLock()

  private lazy val localProps: Properties = {
    val prop = new Properties()
		val stream: InputStream = getClass.getResourceAsStream("/server.conf")
    if (stream != null) try {
      println("read server properties")
      prop.load(stream)
    } finally stream.close()
    prop
  }

  def getServerProperty(key: String): Option[String] = {
    //Logger.debug("getServerProperty: " + key)
    val v = localProps.getProperty(key)
    //Logger.debug("getServerProperty: " + key + " : "+ v)
    Option(v)
  }

	val tinylogfile = new File("./base/tinylog.properties")

	{
		val configurator = if (tinylogfile.exists())
			Configurator.fromFile(tinylogfile)
		else
			Configurator.fromResource("tinylogfile.properties")

		if (getServerProperty("as_box_client").contains("true"))
			configurator.addWriter(new PostWriter()).activate()
		else
			configurator.activate()
	}

	var server: Option[NettyServer] = None
	var filters: Seq[Filter[RemoteNettyHttpRequest, Response, RemoteNettyHttpRequest, Response]] = Seq.empty
	private var controllers: Stream[Controller] = Stream(new CometAndAjaxController)//new CdApp()

	def addFilter(filter: Filter[RemoteNettyHttpRequest, Response, RemoteNettyHttpRequest, Response]): Unit = {
		filters = filters ++ Seq(filter)
	}

	def allFilters(baseService: Service[RemoteNettyHttpRequest, Response]):
		Service[RemoteNettyHttpRequest, Response] = {
		filters.foldRight(baseService) { (b,a) => b andThenS a }
	}

	private[this] lazy val service = {
		val appService = new Service[RemoteNettyHttpRequest, Response] {
			def apply(rawRequest: RemoteNettyHttpRequest): Future[Response] = {
				attemptRequest(rawRequest).recover {
					case t: Throwable =>
						//Await.result(errorHandler(rawRequest, t), Timeout(5 seconds).duration)
						errorHandler(rawRequest, t)
				}
				/*
				try {
					Logger.debug("run service")
					attemptRequest(rawRequest).recover {
						case t: Throwable =>
							Await.result(errorHandler(rawRequest, t), Timeout(5 seconds).duration)
					}
				} catch {
					case e: Exception =>
						Logger.error(e)
						errorHandler(rawRequest, e)
				}
				 */
			}
		}


		//val loggingFilter = new LoggingFilter
		//addFilter(loggingFilter)


		if (!getServerProperty("no_file_service").contains("true")) {
			val fileService = new FileService
			addFilter(fileService)
		}

		allFilters(appService)
	}

	def notFoundHandler(request:RemoteNettyHttpRequest):Future[Response]= {
	  val rb = new ResponseBuilder
	  val rbf =rb.status(404).plain("Not Found").toFuture
	  rbf.map(_.build(request))
	}

	def errorHandler(request:RemoteNettyHttpRequest, e:Throwable): Response = {
		e.printStackTrace()
	  val rb = new ResponseBuilder
	  rb.status(500).plain("Exception:"+e.getMessage).build(request)
	  //rbf.map(_.build(request))
	}


	def attemptRequest(rawRequest: RemoteNettyHttpRequest): Future[Response] = {
		Logger.debug("attemptRequest: " + controllers.size)
		rwl.readLock().lock()
		val resp = controllers.map(_.route(rawRequest)).find(_.isDefined).flatten match {
			case Some(response) =>
				response
			case None =>
				notFoundHandler(rawRequest)
		}
		rwl.readLock().unlock()
		resp
	}
	private[server] def attemptDataRequest(request: Request,
																				 withData: Boolean = false): Future[Seq[DataResponse]] = {
		val ed = controllers.flatMap(_.getExtraData(request, withData))
		controllers.map(_.routeData(request, withData)).find(_.isDefined).flatten match {
			case Some(response) =>
				Future.sequence(response +: ed)
			case None =>
				Logger.debug("Can't find request processor: " + request.path)
				Logger.debug(ed)
				Future.sequence(Future{DataResponse(request.path)} +: ed)
		}
	}
	private[server] def fillDataRequestRouteParams(request: Request): Unit = {
		controllers.foreach(_.routeDataFillParams(request))
	}


	def main(controllers: Seq[Controller], bootstrapper: Bootstrapper, preInit: () => Unit): Unit = {
	  try {
		  BootstrapDI.bootstrapper = Some(bootstrapper)
			Logger.info("developer mode: "+ bootstrapper.isDevMode)
	    val start = System.currentTimeMillis()
	    preInit()
	    //PrintHelper.initPrint()
			val startSrv = System.currentTimeMillis()
			//FileService.init

			Logger.info("mindweb http server starting...")
			server = Some(new NettyServer())
			val port = getServerProperty("port").flatMap(p => Helpers.tryo{p.toInt}).getOrElse(8080)
			server.get.serve(port, service)
			Logger.info("mindweb started on port " + port)
			Logger.info("server started in "+((System.currentTimeMillis() - startSrv)/1000f)+" seconds")

			/*
			if (getServerProperty("as_box_client").contains("true"))
				HttpClientUtils.init()
			 */

			Logger.debug("super_session: " + MindwebServer.getServerProperty("super_session").contains("true"))
			Logger.debug("with_user: " + MindwebServer.getServerProperty("with_user").contains("true"))
			if (MindwebServer.getServerProperty("super_session").contains("true") &&
					MindwebServer.getServerProperty("with_user").contains("true")) {
				Future{
					NotificationsServer.start()
				}(ServerContext.task_dispatcher)
			}

			bootstrapper.init(server.map(s => new ClosableWrapper() {
			  def close(after: FiniteDuration): Future[Unit] = {
					Logger.info("closing server after: " + after)
					val promise = Promise[Unit]()
					system.scheduler.scheduleOnce(after, new Runnable {
						override def run(): Unit = {
							Logger.debug("Run close")
							NotificationsServer.stop()
							s.stop()
							ServerContext.system.terminate()
							CometContext.system.terminate()
							DefaultSystemContext.system.terminate()
							Logger.info("server is closed")
							promise.success("done")
						}
					})(ServerContext.task_dispatcher)
					promise.future
			  }
			}))

			//Load sitemap
			Logger.debug("Try load sitemap")
			val ajaxSiteMap = SiteMap.fromJson(System.getProperty("com.nn.regbox.sitemapRoot"))
			ajaxSiteMap.parseMap()

			ajaxSiteMap.onReload(() => {
				rwl.writeLock().lock()
				val spanned = this.controllers.toList.span(!_.isInstanceOf[AjaxMapController])
				this.controllers =
					(spanned._1 ++ Seq(ajaxSiteMap.controller()) ++ spanned._2.span(_.isInstanceOf[AjaxMapController])._2).toStream
				rwl.writeLock().unlock()
			})

			addControllers(Seq(ajaxSiteMap.controller()))
			addControllers(controllers)


			Logger.info("Whole system started in "+((System.currentTimeMillis() - start)/1000f)+" seconds")
	
			//com.twitter.util.Await.ready(server.get)
		} catch {
		  case e: Throwable =>
				//e.printStackTrace()
				Logger.error(e)
				Logger.info("Should close")
				server.foreach(_.stop())
				NotificationsServer.stop()
				ServerContext.system.terminate()
				CometContext.system.terminate()
				DefaultSystemContext.system.terminate()
				Logger.info("stopped")
		}
		Logger.info("Done")
	}

	def addControllers(controllers: Seq[Controller]): Unit = {
		this.controllers = this.controllers.append(controllers)
	}
}