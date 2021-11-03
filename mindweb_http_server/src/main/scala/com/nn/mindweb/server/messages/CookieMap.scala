package com.nn.mindweb.server.messages

import scala.collection.mutable
import java.util.Date

import com.nn.mindweb.server.Request
import org.pmw.tinylog.Logger

import scala.collection.JavaConverters._

class CookieMap(message: CookieMessage)
  extends mutable.Map[String, Cookie]
  with mutable.MapLike[String, Cookie, CookieMap] {
  override def empty: CookieMap = new CookieMap(Request())

  private[this] val underlying = mutable.Map[String, Seq[Cookie]]()

  /** Check if there was a parse error. Invalid cookies are ignored. */
  def isValid: Boolean = _isValid
  private[this] var _isValid = true

  private[this] val cookieHeaderName =
    if (message.isRequest)
      HeaderNames.COOKIE
    else
      HeaderNames.SET_COOKIE

  private[this] def decodeCookies(header: String): Iterable[Cookie] = {
    Logger.debug("decodeCookies: " + header)
    try {
      Cookie.decode(header)
    } catch {
      case _: IllegalArgumentException =>
        _isValid = false
        Nil
    }
  }

  protected def rewriteCookieHeaders() {
    // Clear all cookies - there may be more than one with this name.
    message.headers().remove(cookieHeaderName)

    // Add cookies back again
    foreach { case (_, cookie) =>
      val encodedCookie = if (message.isResponse) {
        CookieEncoder.encodeServerSide(cookie)
      } else {
        CookieEncoder.encodeClientSide(cookie)
      }
      message.headers().add(cookieHeaderName, encodedCookie)
    }
  }
  

  /** Iterate through all cookies. */
  def iterator: Iterator[(String, Cookie)] = {
    for {
      (name, cookies) <- underlying.iterator
      cookie <- cookies
    } yield (name, cookie)
  }

  /** Get first cookie with this name. */
  def get(key: String): Option[Cookie] = getAll(key).headOption
  def getValue(key: String): Option[String] = get(key) map { _.value }

  /** Get all cookies with this name. */
  def getAll(key: String): Seq[Cookie] = underlying.getOrElse(key, Nil)

  /** Add cookie. Remove existing cookies with this name. */
  def +=(kv: (String, Cookie)): CookieMap.this.type = {
    underlying(kv._1) = Seq(kv._2)
    rewriteCookieHeaders()
    this
  }

  def +=(cookie: Cookie): CookieMap = {
    this += ((cookie.name, cookie))
  }

  /** Delete all cookies with this name. */
  def -=(key: String): CookieMap.this.type = {
    underlying -= key
    rewriteCookieHeaders()
    this
  }

  /** Add cookie. Keep existing cookies with this name. */
  def add(k: String, v: Cookie): Unit = {
    underlying(k) = underlying.getOrElse(k, Nil) :+ v
    rewriteCookieHeaders()
  }

  def add(cookie: Cookie): Unit = {
    add(cookie.name, cookie)
  }

  for {
    cookieHeader <- message.headers().getAll(cookieHeaderName).asScala
    cookie <- decodeCookies(cookieHeader)
  } {
    //Logger.debug("add cookie: " + cookie)
    add(cookie)
  }
}

object CookieEncoder {
  val EQUALS: Byte = 61
  val SEMICOLON: Byte = 59
  val SP: Byte = 32
  val DOUBLE_QUOTE: Byte = '"'
  val COMMA: Byte = 44
    
  def encodeServerSide(cookie: Cookie): String = {
    val sb = new StringBuilder()
    
    add(sb, cookie.name, cookie.value)
    
    if (cookie.maxAge.toSeconds != Long.MinValue) {
      if (cookie.version == 0) {
        addUnquoted(sb, CookieHeaderNames.EXPIRES,
                  HttpHeaderDateFormat.get().format(
                          new Date(System.currentTimeMillis() +
                                   cookie.maxAge.toMillis)))
      } else {
        add(sb, CookieHeaderNames.MAX_AGE, cookie.maxAge.toSeconds.toString)
      }
    }

    if (cookie.path != null) {
      if (cookie.version > 0) {
        add(sb, CookieHeaderNames.PATH, cookie.path)
      } else {
        addUnquoted(sb, CookieHeaderNames.PATH, cookie.path)
      }
    }

    if (cookie.domain != null) {
      if (cookie.version > 0) {
          add(sb, CookieHeaderNames.DOMAIN, cookie.domain)
      } else {
          addUnquoted(sb, CookieHeaderNames.DOMAIN, cookie.domain)
      }
    }
    if (cookie.isSecure) {
      sb.append(CookieHeaderNames.SECURE)
      sb.append(SEMICOLON.toChar)
      sb.append(SP.toChar)
    }
    //only for https
    //addUnquoted(sb, CookieHeaderNames.SAME_SITE, "None")
    if (cookie.httpOnly) {
      sb.append(CookieHeaderNames.HTTPONLY)
      sb.append(SEMICOLON.toChar)
      sb.append(SP.toChar)
    }
    if (cookie.version >= 1) {
      if (cookie.comment != null) {
        add(sb, CookieHeaderNames.COMMENT, cookie.comment)
      }

      add(sb, CookieHeaderNames.VERSION, 1.toString)

      if (cookie.commentUrl != null) {
        addQuoted(sb, CookieHeaderNames.COMMENTURL, cookie.commentUrl)
      }

      if (cookie.ports.nonEmpty) {
        sb.append(CookieHeaderNames.PORT)
        sb.append(EQUALS.toChar)
        sb.append(DOUBLE_QUOTE.toChar)
        cookie.ports.foreach(port => {
          sb.append(port)
          sb.append(COMMA.toChar)
        })
        sb.setCharAt(sb.length() - 1, DOUBLE_QUOTE.toChar)
        sb.append(SEMICOLON.toChar)
        sb.append(SP.toChar)
      }
      if (cookie.isDiscard) {
        sb.append(CookieHeaderNames.DISCARD)
        sb.append(SEMICOLON.toChar)
        sb.append(SP.toChar)
      }
    }

    if (sb.length() > 0) {
      sb.setLength(sb.length() - 2)
    }

    sb.toString()
  }
  
  def encodeClientSide(cookie: Cookie): String = {
    val sb = new StringBuilder()

    if (cookie.version >= 1) {
      add(sb, '$' + CookieHeaderNames.VERSION, 1.toString)
    }

    add(sb, cookie.name, cookie.value)

    if (cookie.path != null) {
      add(sb, '$' + CookieHeaderNames.PATH, cookie.path)
    }

    if (cookie.domain != null) {
      add(sb, '$' + CookieHeaderNames.DOMAIN, cookie.domain)
    }

    if (cookie.version >= 1) {
      if (cookie.ports.nonEmpty) {
        sb.append('$')
        sb.append(CookieHeaderNames.PORT)
        sb.append(EQUALS.toChar)
        sb.append(DOUBLE_QUOTE.toChar)
        cookie.ports.foreach(port => {
          sb.append(port)
          sb.append(COMMA.toChar)
        })
        sb.setCharAt(sb.length() - 1, DOUBLE_QUOTE.toChar)
        sb.append(SEMICOLON.toChar)
        sb.append(SP.toChar)
      }
    }

    if (sb.length() > 0) {
      sb.setLength(sb.length() - 2)
    }
    sb.toString()
  }
  
  private def add(sb: StringBuilder, name: String, v: String): Unit = {
    if (v == null) {
      addQuoted(sb, name, "")
      return
    }
    //var i = 0
    v.foreach(c => {
      if ((c == '\t') || (c == ' ') || (c == '"') || (c == '(') || (c == ')') || (c == ',') ||
          (c == '/') || (c == ':') || (c == ';') || (c == '<') || (c == '=') || (c == '>') ||
          (c == '?') || (c == '@') || (c == '[') || (c == '\\') || (c == ']') || (c == '{') || (c == '}')) {
	      addQuoted(sb, name, v)
	      return
      }
    })

    addUnquoted(sb, name, v)
  }

  private def addUnquoted(sb: StringBuilder, name: String, v: String): Unit = {
    sb.append(name)
    sb.append(EQUALS.toChar)
    sb.append(v)
    sb.append(SEMICOLON.toChar)
    sb.append(SP.toChar)
  }

  private def addQuoted(sb: StringBuilder, name: String, v: String) = {
    val value = if (v == null) "" else v

    sb.append(name)
    sb.append(EQUALS.toChar)
    sb.append(DOUBLE_QUOTE.toChar)
    sb.append(value.replace("\\", "\\\\").replace("\"", "\\\""))
    sb.append(DOUBLE_QUOTE.toChar)
    sb.append(SEMICOLON.toChar)
    sb.append(SP.toChar)
  }
/*
    private static void add(StringBuilder sb, String name, int val) {
        sb.append(name);
        sb.append((char) HttpConstants.EQUALS);
        sb.append(val);
        sb.append((char) HttpConstants.SEMICOLON);
        sb.append((char) HttpConstants.SP);
    }
    * 
    */
}