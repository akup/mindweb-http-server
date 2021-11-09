package com.nn.mindweb.server.messages

import io.netty.handler.codec.http.{DefaultHttpHeaders, HttpHeaders, HttpMessage, HttpVersion}

import java.util.{Iterator => JIterator}
import scala.collection.JavaConverters._

trait CookieMessage extends HttpMessage {
/*
  private[this] val readerWriter = BufReader.writable()

  /**
   * A read-only handle to the internal stream of bytes, representing the
   * message body. See [[com.twitter.util.Reader]] for more information.
   **/
  def reader: BufReader = readerWriter

  /**
   * A write-only handle to the internal stream of bytes, representing the
   * message body. See [[com.twitter.util.Writer]] for more information.
   **/
  def writer: BufWriter = readerWriter
*/
  def isRequest: Boolean
  def isResponse: Boolean = !isRequest

  //def content: ChannelBuffer = getContent()
  //def content_=(content: ChannelBuffer) { setContent(content) }

  var http_version: HttpVersion
  @deprecated
  override def getProtocolVersion: HttpVersion = http_version
  override def protocolVersion: HttpVersion = http_version
  override def setProtocolVersion(version: HttpVersion): HttpMessage = {
    http_version = version
    this
  }
  val _headers: DefaultHttpHeaders = new DefaultHttpHeaders()
  override def headers(): HttpHeaders = _headers

  //lazy val headerMap: HeaderMap = new MapHeaderMap(mutable.Map.empty)
  // Java users: use Netty HttpHeaders interface for headers

  /**
   * Cookies. In a request, this uses the Cookie headers.
   * In a response, it uses the Set-Cookie headers.
   */
  lazy val cookies = new CookieMap(this)
  // Java users: use the interface below for cookies

  /** Get iterator over Cookies */
  def getCookies: JIterator[Cookie] = cookies.valuesIterator.asJava

  /** Add a cookie */
  def addCookie(cookie: Cookie): Unit = {
    cookies += cookie
  }

  /** Remove a cookie */
  def removeCookie(name: String): Unit = {
    cookies -= name
  }
}

/*
object Message {
  private[messages] val Utf8          = Charset.forName("UTF-8")
  @deprecated("Use MediaType.Json", "6.1.5")
  val MediaTypeJson         = "application/json"
  @deprecated("Use MediaType.Javascript", "6.1.5")
  val MediaTypeJavascript   = "application/javascript"
  @deprecated("Use MediaType.WwwForm", "6.1.5")
  val MediaTypeWwwForm      = "application/x-www-form-urlencoded"
  val CharsetUtf8           = "charset=utf-8"
  val ContentTypeJson       = MediaType.Json + ";" + CharsetUtf8
  val ContentTypeJavascript = MediaType.Javascript + ";" + CharsetUtf8
  val ContentTypeWwwFrom    = MediaType.WwwForm + ";" + CharsetUtf8

  private val HttpDateFormat = RichDateFormat.getInstance("EEE, dd MMM yyyy HH:mm:ss",
                                                          TimeZone.getTimeZone("GMT"))
  def httpDateFormat(date: Date): String =
    HttpDateFormat.format(date) + " GMT"
}
 */