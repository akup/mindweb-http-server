/**
* Copyright (C) 2012 Twitter Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package com.nn.mindweb.server

import java.text.SimpleDateFormat
import java.util.Date
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import io.netty.handler.codec.http.{HttpResponseStatus, HttpVersion}
import net.aklabs.regbox.must.View
import org.pmw.tinylog.Logger

import scala.concurrent.Future

import messages._
import netty._
import ServerContext._

object ResponseBuilder {
	def apply(body: String): Response =
		new ResponseBuilder().body(body).status(200).build

	def apply(status: Int, body: String): Response =
		new ResponseBuilder().body(body).status(status).build
	
	def apply(status: Int, body: String, headers: Map[String, String]): Response =
		new ResponseBuilder().body(body).status(status).headers(headers).build
		
	lazy val formatter = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz")
	lazy val lastModDate: String = formatter.format(new Date())
	lazy val expire: Date = {
	    val dt = new Date()
	    dt.setYear(dt.getYear+1)
	    dt
    }
	lazy val expireDate = formatter.format(expire)
}

class ResponseBuilder /*extends CommonStatuses*/ {

	private var status: Option[HttpResponseStatus] = None
	private var statusCode: Option[Int] = None
	private var headers: Map[String, String] = Map()
	private var strBody: Option[String] = None
	private var binBody: Option[Array[Byte]] = None
	private var json: Option[Any] = None
	private var view: Option[View] = None
	private var cookies: List[Cookie] = List()

	private lazy val jsonMapper = {
	  Logger.info("json mapper initializing...")
		val m = new ObjectMapper()
		m.registerModule(DefaultScalaModule)
	}
	
	def contentType: Option[String] = this.headers.get("Content-Type")

	private def setContent(resp: Response): Response = {
		json match {
			case Some(j) =>
				resp.headers().add("Content-Type", "application/json")
				val jsonBytes = jsonMapper.writeValueAsString(j).getBytes("UTF-8")
				resp.headers().add("Content-Length", jsonBytes.length.toString)
				resp.content = jsonBytes
			case None =>
				view match {
					case Some(v) =>
						val t1 = System.nanoTime()
						val out = v.render
						val t2 = System.nanoTime()
						val bytes = out.getBytes("UTF-8")
						resp.headers().add("Content-Length", bytes.length.toString)
						if (v.contentType.isDefined && !resp.headers().contains("Content-Type")) {
							resp.headers().add("Content-Type", v.contentType.get)
						}
						resp.content = bytes
						val t3 = System.nanoTime()
						Logger.info("RENDER VIEW TIME: " + (t2 - t1))
						Logger.info("CREATE RESPONSE TIME: " + (t3 - t2))
					case None =>
						strBody match {
							case Some(sb) =>
								val bytes = sb.getBytes("UTF-8")
								resp.headers().add("Content-Length", bytes.length.toString)
								resp.content = bytes
							case None =>
								binBody match {
									case Some(bb) =>
										resp.headers().add("Content-Length", bb.length.toString)
										resp.content = bb
									case None => resp.headers().add("Content-Length", "0") //no content
								}
						}
				}
		}
		resp
	}

	def cookie(k: String, v: String): ResponseBuilder = {
		this.cookies ::= new Cookie(k, v)
		this
	}

	def cookie(c: Cookie): ResponseBuilder = {
		this.cookies ::= c
		this
	}

	def body(s: String): ResponseBuilder = {
		this.strBody = Some(s)
		this
	}

	def status(i: Int): ResponseBuilder = {
		this.statusCode = Some(i)
		this
	}
	def status(s: HttpResponseStatus): ResponseBuilder = {
		this.status = Some(s)
		this
	}

	def nothing: ResponseBuilder = {
		this.header("Content-Type", "text/plain")
		this.body("")
		this
	}

	def plain(body:String): ResponseBuilder = {
		this.header("Content-Type", "text/plain")
		this.body(body)
		this
	}

	def html(body:String): ResponseBuilder = {
		this.header("Content-Type", "text/html")
		this.body(body)
		this
	}

	def body(b: Array[Byte]): ResponseBuilder = {
		this.binBody = Some(b)
		this
	}

	def header(k: String, v: String): ResponseBuilder = {
		this.headers += (k -> v)
		this
	}

	def headers(m: Map[String, String]): ResponseBuilder = {
		this.headers = this.headers ++ m
		this
	}

	def json(o: Any): ResponseBuilder = {
		this.header("Content-Type", "application/json")
		this.json = Some(o)
		this
	}
	def accessControlOrigin(req: Request): ResponseBuilder = {
		req.params.get("host") match {
			case Some(host) if host != "null" =>
				this.headers += ("Access-Control-Allow-Origin" -> host)
				this.headers += ("Access-Control-Allow-Credentials" -> "true")
			case _ =>
				this.headers += ("Access-Control-Allow-Origin" -> "*")
		}
		this
	}

	def view(v: View): ResponseBuilder = {
		this.view = Some(v)
		this
	}

	def contentType(ct: String): ResponseBuilder = {
		this.header("Content-Type", ct)
		this
	}

	def build: Response = {
			build(Request())
	}

	def build(request: RemoteNettyHttpRequest): Response = {
		val response = Response(request.request.protocolVersion())
		build(response)
	}
	def build(request: Request): Response = {
		val response = Response(request.getProtocolVersion)
		build(response)
	}
	def build(response: Response): Response = {
		// Only set the status code if set explicitly in the builder
		this.statusCode foreach response.setStatusCode
		this.status foreach response.setStatus

		headers.foreach { xs =>
			response.headers().add(xs._1, xs._2)
		}
		//Logger.debug("Cumulative headers: " + response.headers().asScala.toList)
		//Logger.debug("headers: " + headers)
		cookies foreach response.cookies.add
		setContent(response)
		//Logger.debug("Cumulative headers2: " + response.headers().asScala.toList)
		response
	}

	def toFuture:Future[ResponseBuilder] = Future{this}//Future.value(this)
	
	override def toString: String = {
		val buf = new StringBuilder
		buf.append(getClass.getSimpleName)
		buf.append('\n')
		buf.append(HttpVersion.HTTP_1_1.toString)
		buf.append(' ')
		buf.append(this.statusCode)
		buf.append('\n')
		buf.append(this.headers)
		buf.toString()
	}


	def notFound(msg: Option[String] = None, template: Option[String] = None): ResponseBuilder = {
		this.statusCode = Some(404)
		this.view(new View {
			override def templateFileName: Option[String] = template.orElse(Some("error_pages/404.mustache"))

			val message: Option[String] = msg
		})

		this
	}
	def internalError(msg: Option[String] = None, template: Option[String] = None): ResponseBuilder = {
		this.statusCode = Some(500)
		try {
			throw new Exception("trace internal error")
		} catch {
			case e: Throwable =>
				e.printStackTrace()
		}
		this.view(new View {
			override def templateFileName: Option[String] = template.orElse(Some("error_pages/500.mustache"))

			val message: Option[String] = msg
		})

		this
	}
	def redirect(redirectTo: String): ResponseBuilder = {
		this.statusCode = Some(302)
		this.header("Location", redirectTo)
		this
	}
}

/*trait CommonStatuses { self: ResponseBuilder =>
	def ok: ResponseBuilder = buildFromStatus(Status.Ok)
	def created: ResponseBuilder = buildFromStatus(Status.Created)
	def accepted: ResponseBuilder = buildFromStatus(Status.Accepted)
	def movedPermanently: ResponseBuilder = buildFromStatus(Status.MovedPermanently)
	def found: ResponseBuilder = buildFromStatus(Status.Found)
	def notModified: ResponseBuilder = buildFromStatus(Status.NotModified)
	def temporaryRedirect: ResponseBuilder = buildFromStatus(Status.TemporaryRedirect)
	def badRequest: ResponseBuilder = buildFromStatus(Status.BadRequest)
	def unauthorized: ResponseBuilder = buildFromStatus(Status.Unauthorized)
	def forbidden: ResponseBuilder = buildFromStatus(Status.Forbidden)
	def notFound: ResponseBuilder = buildFromStatus(Status.NotFound)
	def gone: ResponseBuilder = buildFromStatus(Status.Gone)
	def internalServerError: ResponseBuilder = buildFromStatus(Status.InternalServerError)
	def notImplemented: ResponseBuilder = buildFromStatus(Status.NotImplemented)
	def serviceUnavailable: ResponseBuilder = buildFromStatus(Status.ServiceUnavailable)

	private def buildFromStatus(st: HttpResponseStatus): ResponseBuilder = {
		status(st.getCode)
		this
	}
}*/