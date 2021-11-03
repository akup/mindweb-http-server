package com.nn.mindweb.server

import com.aklabs.login._
import com.fasterxml.jackson.databind.JsonNode
import com.nn.http.{ContinueResponseException, MapParamMap, R, RoundTripInfo, StreamFileParamHolder, TheSession, HttpRequest => SessRequest}
import com.nn.mindweb.server.messages.Response
import com.nn.mindweb.server.netty.RemoteNettyHttpRequest
import io.netty.handler.codec.http.HttpMethod
import net.aklabs.helpers.JsonHelpers._

import scala.collection.JavaConverters._
import org.pmw.tinylog.Logger

import scala.collection.immutable
import scala.collection.immutable.{Map, Vector}
import scala.concurrent.Future


case class DataResponse(path: String,
												template_js: Map[String, _] = Map.empty,
												data_js: Map[String, _] = Map.empty,
												language: Option[String] = R.httpSession.flatMap(_.attribute("locale").map(_.toString)),
												loc_res: Option[String] = None,
												roundTrip: Seq[RoundTripInfo] = Nil,
												var usedD: Boolean = false)
/**
 * Контроллер: занимается тем, что хранитпроцедуры обработки каждого маршрута,
 * и разбирает входящий запрос, передавая его на обработку соответствующей процедуре.
 */
class Controller {
	import ServerContext._

	type routeElement = (HttpMethod, PathPattern, Boolean, Request => Future[ResponseBuilder])
	type rtVector = Vector[routeElement]
	type dataRouteElement = (PathPattern, (Request, Boolean) => Future[DataResponse])
	type drtVector = Vector[dataRouteElement]

	var getRoutes : rtVector = Vector()
	var postRoutes : rtVector = Vector()

	var dataRoutes : drtVector = Vector()
	var extraData : Vector[(Request, Boolean) => Option[Future[DataResponse]]] = Vector()

	var notFoundHandler: Option[Request => Future[ResponseBuilder]] = None
	var errorHandler: Option[Request => Future[ResponseBuilder]] = None

	def get(path: String, stateful: Boolean = true) (callback: Request => Future[ResponseBuilder]) {
	  getRoutes :+= addRoute(HttpMethod.GET, path, stateful)(callback)
	}
	def post(path: String, stateful: Boolean = true) (callback: Request => Future[ResponseBuilder]) {
	  postRoutes :+= addRoute(HttpMethod.POST, path, stateful)(callback)
	}
	def jsonData(path: String) (callback: (Request, Boolean) => Future[DataResponse]) {
		val regex = SinatraPathPatternParser(path)
		dataRoutes :+= (regex, (r: Request, withData: Boolean) => callback(r, withData))
	}
	def extraDataAdd(f: (Request, Boolean) => Option[Future[DataResponse]]): Unit = {
		extraData :+= f
	}

	/*def delete(path: String)(callback: FinagleRequest => Future[ResponseBuilder]) { addRoute(HttpMethod.DELETE, path)(callback) }
	def put(path: String) (callback: FinagleRequest => Future[ResponseBuilder]) { addRoute(HttpMethod.PUT, path)(callback) }
	def head(path: String) (callback: FinagleRequest => Future[ResponseBuilder]) { addRoute(HttpMethod.HEAD, path)(callback) }
	def patch(path: String) (callback: FinagleRequest => Future[ResponseBuilder]) { addRoute(HttpMethod.PATCH, path)(callback) }
	def options(path: String)(callback: FinagleRequest => Future[ResponseBuilder]){ addRoute(HttpMethod.OPTIONS, path)(callback) }*/


	def addRoute(method: HttpMethod, path: String, stateful: Boolean)(callback: Request => Future[ResponseBuilder]) :routeElement= {
		val regex = SinatraPathPatternParser(path)
		(method, regex, stateful, (r: Request) => callback(r))
	}

	def notFound(callback: Request => Future[ResponseBuilder]) {
		notFoundHandler = Option(callback)
	}

	def error(callback: Request => Future[ResponseBuilder]) {
		errorHandler = Option(callback)
	}

	//val stats = statsReceiver.scope("Controller")
	def render: ResponseBuilder = new ResponseBuilder

	def redirect(location: String, message: String = "", permanent: Boolean = false): ResponseBuilder = {
		val msg = if (message == ""){
			"Redirecting to <a href=\"%s\">%s</a>.".format(location, location)
		}else{
			message
		}
		val code = if (permanent) 301 else 302
		render.plain(msg).status(code).header("Location", location)
	}

	private def doSuperSession(r: SessRequest, s: TheSession, continue: Option[() => Nothing], f: () => Future[Response]): Future[Response] = {
    try {
			Logger.debug("R.init")
      R.init(r, s) {

        //ControlHelpers.tryo {
        try {
        	TheSession.onBeginServicing.foreach(_(s, r))
        } catch {
          case e: Throwable => e.printStackTrace()
        }

				s.restored.get("edgeuser") match {
					case Some(user: EdgeUser) =>
						EdgeUser.logUserIn(user)
					case _ =>
				}
				s.restored = Map.empty

        if (MindwebServer.getServerProperty("with_user").contains("true") &&
						!EdgeUser.loggedIn_?) {
					EdgeUser.loginByCookies()

					//TODO: restore user by session
        }

        //makeCometBreakoutDecision(s, r)
	      //}

        if (!r.path.startsWith("/mw_ajax/")) {
          Logger.debug("not ajax: " + r.path)
        	R.session.foreach(_.runParams(r))
        }
        val resp = f()

        if (R.functionMap.nonEmpty) {
          s.updateFunctionMap(R.functionMap, R.renderVersion, System.currentTimeMillis)
          R.clearFunctionMap()
        }
        //TheSession.onEndServicing.foreach(_(s, r, resp))

        resp
        //dispatchStatefulRequest(S.request.openOrThrowException("I'm pretty sure this is a full box here"), liftSession, r2, continue)
      }
    } catch {
      case cre: ContinueResponseException =>
        throw cre
        /*
        s2.st
        r2.destroyServletSession()
        doSession(r2, getLiftSession(r2), Full(cre.continue))
        */
    }
  }

	/**
	 * Маршрутизация
	 */
	def route(request :RemoteNettyHttpRequest): Option[Future[Response]] = {
		Logger.debug("Try route request by " + this)
		findRouteAndMatch(request).flatMap{case (_, routeParams, stateful, callback) => {
		  Logger.debug("got route")
		  val req = new Request(request, stateful)

			request.synchronized{
				request._addPostParam = Some((name, value) => {
					val underlying = req.mapParams.keySet.map(key => {
						key -> req.mapParams.getAll(key).toSeq
					}).toMap + (name -> Seq(value))
					req.mapParams = new MapParamMap(underlying = underlying)
				})
				request._addFile = Some((name, fInfoFunc) => {
					if (!req.uploadingfiles.isDefinedAt(name)) {
						val finfo = fInfoFunc()
						req.uploadingfiles += name -> new StreamFileParamHolder(name = name,
							mimeType = finfo.mimeType,
							fileName = finfo.fileName,
							dataIn = finfo.data,
							size = finfo.length)
					}
				})
				request._completeFile = Some(name => {
					Logger.debug("complete file: " + name)
					req.uploadingfiles.get(name).foreach(_.complete = true)
				})
			}

		  routeParams.foreach(_.foreach(req.routeParams += _))

		  def processReq(): Future[Response] = {
		    //Logger.debug("processReq")
		    callback(req).map(rb => {
			    req.session.map(s => rb.cookie("sessionid", s.sessionId)).getOrElse(rb).build(request)
			  })
		  }

		  Logger.debug("before process req: " + MindwebServer.getServerProperty("super_session"))
		  Some(
			  if (MindwebServer.getServerProperty("super_session").contains("true")) {
			    Logger.debug("super session")
			    if (req.stateless_?)
			      R.statelessInit(req)(processReq())
			    else
			    	doSuperSession(req, SessionMaster.getTheSession(req), None, processReq)
			  } else
			  	processReq()
		  	)
		}}
	}

	def routeData(req: Request, withData: Boolean): Option[Future[DataResponse]] = {
		var thematch: Option[Map[_,_]] = None
		dataRoutes.find(route => {
			thematch = route._1(req.uri.split('?').head)
			thematch.isDefined
		}).map(route => {
			//val routeParams = thematch.map(_.collect{case (x: String, y: Seq[String]) if y.nonEmpty => x -> y.head})
			//routeParams.foreach(_.foreach(req.routeParams += _))
			route._2(req, withData)
		})
	}
	def routeDataFillParams(req: Request): Unit = {
		var thematch: Option[Map[_,_]] = None
		dataRoutes.find(route => {
			thematch = route._1(req.uri.split('?').head)
			thematch.isDefined
		}).foreach(_ => {
			val routeParams = thematch.map(_.collect{case (x: String, y: Seq[String]) if y.nonEmpty => x -> y.head})
			routeParams.foreach(_.foreach(req.routeParams += _))
		})
	}
	def getExtraData(req: Request, withData: Boolean): Seq[Future[DataResponse]] = {
		extraData.flatMap(ed => ed(req, withData))
	}

	def findRouteAndMatch(r: RemoteNettyHttpRequest):
			Option[(HttpMethod, Option[Map[String, String]], Boolean, Request => Future[ResponseBuilder])] = {
	  val routes : rtVector = r.request.method match{
		  case HttpMethod.GET => getRoutes
		  case HttpMethod.POST => postRoutes
		}
		var thematch: Option[Map[_,_]] = None
		//Logger.debug("FIND ROUTE: " + r.request.uri().split('?').head)
		//Logger.debug(routes)
		routes.find( route => {
			thematch = route._2(r.request.uri().split('?').head)
			thematch.isDefined
		}).map(route => {
		  //Logger.info("FOUND ROUTE: " + route._1)
      (route._1, thematch.map(_.collect{case (x: String, y: Seq[String]) if y.nonEmpty => x -> y.head}), route._3, route._4)
		})
	}

	def formatResponse(req: RemoteNettyHttpRequest, resp: Future[ResponseBuilder]): Future[Response] = {
			resp.map(_.build(req))
	}

	def respondTo(r: RemoteNettyHttpRequest)(callback: PartialFunction[ContentType, Future[ResponseBuilder]]): Future[ResponseBuilder] = {
		val contentType = new ContentType.All
		if (callback.isDefinedAt(contentType)) {
			callback(contentType)
		} else {
			throw new UnsupportedMediaType
		}
	  /*if (!r.routeParams.get("format").isEmpty) {
			val format = r.routeParams("format")
			val mime = extMap.getContentType("." + format)
			val contentType = ContentType(mime).getOrElse(new ContentType.All)
			if (callback.isDefinedAt(contentType)) {
				callback(contentType)
			} else {
				throw new UnsupportedMediaType
			}
		} else {
			val accepts: Seq[ContentType] ={
				val accept = r.headers.get("Accept")
				if (accept != null) {
					var acceptParts = Splitter.on(',').splitToList(accept)
					//var acceptParts = Splitter.on(',').split(accept).toArray
					//Sorting.quickSort(acceptParts)(AcceptOrdering)
					val arr = acceptParts.toArray().asInstanceOf[Array[String]]
					val seq = arr.map { xs =>
					  	val part = Splitter.on(";q=").splitToList(xs).toArray.asInstanceOf[Array[String]]
						//val part = Splitter.on(";q=").splitToList(xs).toArray.head
						ContentType(part.head).getOrElse(new ContentType.All)
					}.toSeq
					seq
				} else {
					Seq.empty[ContentType]
				}
			}
			accepts.find { mimeType =>
				callback.isDefinedAt(mimeType)
			} match {
				case Some(contentType) => callback(contentType)
				case None => throw new UnsupportedMediaType
			}
		}*/
	}

	protected def jsonDataToStringMap(data: JsonNode): Map[String, String] = data match {
		case obj: JObject => obj.fields().asScala.flatMap(a => {
			a.getValue match {
				case JString(s) => Some(a.getKey -> s)
				case _ => None
			}
		}).toMap
		case _ => Map.empty
	}
}

object ContentType {
	def apply(str: String): Option[ContentType] =
	contentTypeMap.get(str)

	val contentTypeMap: Map[String, ContentType] = Map(
		"application/json" 			-> new Json,
		"text/html" 				-> new Html,
		"text/plain" 				-> new Txt,
		"application/rss" 			-> new Rss,
		"application/xml" 			-> new Xml,
		"*/*" 						-> new All,
		"application/octet-stream" 	-> new All
	)

	class Html extends ContentType
	class Json extends ContentType
	class Txt extends ContentType
	class Xml extends ContentType
	class Rss extends ContentType
	class All extends ContentType
}

class ContentType

class UnsupportedMediaType extends Exception