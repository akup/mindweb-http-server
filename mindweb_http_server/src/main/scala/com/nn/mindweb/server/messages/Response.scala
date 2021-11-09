package com.nn.mindweb.server.messages

import io.netty.handler.codec.DecoderResult
import io.netty.handler.codec.http.{HttpResponse, HttpResponseStatus, HttpVersion}

import java.io.RandomAccessFile

abstract class Response extends CookieMessage with HttpResponse {
  override def isRequest = false

  private[server] var _status: HttpResponseStatus = HttpResponseStatus.OK
  private[server] var content: Array[Byte] = Array()
  private[server] var file: Option[RandomAccessFile] = None
  private[server] var range: Seq[(Option[Int], Option[Int])] = Nil

  def statusCode: Int = _status.code
  def setStatusCode(value: Int): Unit = {
    _status = HttpResponseStatus.valueOf(value)
  }

  @deprecated
  override def getStatus: HttpResponseStatus = _status
  override def status: HttpResponseStatus = _status
  override def setStatus(status: HttpResponseStatus): HttpResponse = {
    _status = status
    this
  }
  override def setProtocolVersion(version: HttpVersion): Response = {
    http_version = version
    this
  }

  def setContent(content: Array[Byte]): Unit = this.content = content
  def getContent: Array[Byte] = content

  private val _decoderResult = DecoderResult.SUCCESS
  override def decoderResult: DecoderResult = _decoderResult
  @deprecated
  override def getDecoderResult: DecoderResult = _decoderResult
  override def setDecoderResult(decoderResult: DecoderResult): Unit = {}

  /** Encode as an HTTP message */
  def encodeString(): String = {
    /*
    val encoder = new EncoderEmbedder[ChannelBuffer](new HttpResponseEncoder)
    encoder.offer(this)
    val buffer = encoder.poll()
    buffer.toString(Charsets.UTF_8)
    */
    ""
  }

  private var chunked = false
  def setChunked(chunked: Boolean): Unit = this.chunked = chunked
  def isChunked: Boolean = chunked

  /*
  def httpHead: String = {
    val sb = new StringBuilder

    sb.append(http_version.toString).append(" ").append(status.code).append(" ").append(status.reasonPhrase).append("\r\n")
    sb.append("Server: MindWeb Edge/2020-06-20\r\n")
    headers().asScala.foreach(kv => {
      sb.append(kv._1).append(": ").append(kv._2).append("\r\n")
    })
    if (!chunked)
      sb.append("Connection: close\r\n\r\n")
    else
      sb.append("Connection: keep-alive\r\n\r\n")

    sb.mkString
  }
   */

  override def toString: String =
    "Response(\"" + http_version + " " + status + "\")"
}


object Response {

  /*
  def decodeString(s: String): Response = {
    /*
    val decoder = new DecoderEmbedder(
      new HttpResponseDecoder(Int.MaxValue, Int.MaxValue, Int.MaxValue))
    decoder.offer(ChannelBuffers.wrappedBuffer(s.getBytes(Charsets.UTF_8)))
    val httpResponse = decoder.poll().asInstanceOf[HttpResponse]
    assert(httpResponse ne null)
    Response(httpResponse)
    */
    apply()
  }
  */

  def apply(): Response =
    apply(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)

  def apply(version: HttpVersion, status: HttpResponseStatus): Response =
    new Response {
      var http_version: HttpVersion = version
      private var http_status = status
    }

  def apply(status: HttpResponseStatus): Response =
    apply(HttpVersion.HTTP_1_1, status)

  def apply(version: HttpVersion): Response =
    apply(version, HttpResponseStatus.OK)
}