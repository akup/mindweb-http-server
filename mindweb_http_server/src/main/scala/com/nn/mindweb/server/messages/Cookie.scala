package com.nn.mindweb.server.messages

import java.util.concurrent.TimeUnit

import java.text.ParseException

import com.nn.regbox.utils.StringUtils
import net.aklabs.helpers.CookieDecoder

import scala.concurrent.duration.{Duration, FiniteDuration}

class Cookie(var _name: String, var _value: String) {
  private var _comment: String = ""
  private var _commentUrl: String = ""
  private var _domain: String = ""
  private var _maxAge: Duration = new FiniteDuration(365, TimeUnit.DAYS)
  private var _path: String = "/"
  private var _ports: Set[Int] = Set.empty
  private var _version: Int = 0
  private var _httpOnly: Boolean = false
  private var _isDiscard: Boolean = false
  private var _isSecure: Boolean = false

  def comment: String    = _comment
  def commentUrl: String = _commentUrl
  def domain: String     = _domain
  def maxAge: Duration   = _maxAge
  def name: String       = _name
  def path: String       = _path
  def ports: Set[Int]    = _ports//underlying.getPorts.asScala.toSet map { i: Integer => i.intValue }
  def value: String      = _value
  def version: Int       = _version
  def httpOnly: Boolean  = _httpOnly
  def isDiscard: Boolean = _isDiscard
  def isSecure: Boolean  = _isSecure

  def comment_=(comment: String)       { _comment = comment }
  def commentUrl_=(commentUrl: String) { _commentUrl = commentUrl }
  def domain_=(domain: String)         { _domain = domain }
  def maxAge_=(maxAge: Duration)       { _maxAge = maxAge }
  def path_=(path: String)             { _path = path }
  def ports_=(ports: Seq[Int])         { _ports = ports.toSet }
  def value_=(value: String)           { _value = value }
  def version_=(version: Int)          { _version = version }
  def httpOnly_=(httpOnly: Boolean)    { _httpOnly = httpOnly }
  def isDiscard_=(discard: Boolean)    { _isDiscard = discard }
  def isSecure_=(secure: Boolean)      { _isSecure = secure }
}

object Cookie {
  private val COMMA = ',';
  
  def decode(header: String): Set[Cookie] = {
    //val names = new ArrayBuffer()
    //val values = new ArrayBuffer()
    val kv = CookieDecoder.extractKeyValuePairs(header)
    val names = kv.getKey()
    val values = kv.getValue()

    if (names.isEmpty()) return Set.empty[Cookie]

    var i = 0
    var version = 0

    // $Version is the only attribute that can appear before the actual
    // cookie name-value pair.
    if (names.get(0).equalsIgnoreCase(CookieHeaderNames.VERSION)) {
        try {
            version = Integer.parseInt(values.get(0));
        } catch {
          case e: NumberFormatException => // Ignore.
        }
        i = 1;
    }

    if (names.size() <= i) return Set.empty[Cookie]

    var cookies = Set[Cookie]()
    while (i < names.size()) {
      var name = names.get(i)
      var value = values.get(i)
      if (value == null) value = ""

      val c = new Cookie(name, value)

      var discard = false
      var secure = false
      var httpOnly = false
      var comment: String = null
      var commentURL: String = null
      var domain: String = null
      var path: String = null
      var maxAge = Long.MinValue / 1000
      var ports = Seq[Int]()

      var j = i + 1
      while (j < names.size()) {
        name = names.get(j);
        value = values.get(j);
        if (CookieHeaderNames.DISCARD.equalsIgnoreCase(name)) {
          discard = true
        } else if (CookieHeaderNames.SECURE.equalsIgnoreCase(name)) {
          secure = true
        } else if (CookieHeaderNames.HTTPONLY.equalsIgnoreCase(name)) {
          httpOnly = true
        } else if (CookieHeaderNames.COMMENT.equalsIgnoreCase(name)) {
          comment = value
        } else if (CookieHeaderNames.COMMENTURL.equalsIgnoreCase(name)) {
          commentURL = value
        } else if (CookieHeaderNames.DOMAIN.equalsIgnoreCase(name)) {
          domain = value
        } else if (CookieHeaderNames.PATH.equalsIgnoreCase(name)) {
          path = value
        } else if (CookieHeaderNames.EXPIRES.equalsIgnoreCase(name)) {
          try {
            val maxAgeMillis = HttpHeaderDateFormat.get().parse(value).getTime() - System.currentTimeMillis()

            maxAge = (maxAgeMillis / 1000) + {if (maxAgeMillis % 1000 != 0) 1 else 0}
          } catch {
            case e: ParseException => // Ignore.
          }
        } else if (CookieHeaderNames.MAX_AGE.equalsIgnoreCase(name)) {
          maxAge = Integer.parseInt(value)
        } else if (CookieHeaderNames.VERSION.equalsIgnoreCase(name)) {
          version = Integer.parseInt(value)
        } else if (CookieHeaderNames.PORT.equalsIgnoreCase(name)) {
          val portList = StringUtils.split(value, COMMA)
          portList.foreach(s1 => {
            try {
              ports :+= Integer.valueOf(s1).intValue()
            } catch {
              case e: NumberFormatException =>  // Ignore.
            }
          })
        } else {
          j = names.size()
        }
        if (j < names.size()) {
        	j += 1
        	i += 1
        }
      }

      c.version = version
      val bound = Long.MaxValue / 1000000000
      if (maxAge < -bound) maxAge = -bound
      else if (maxAge > bound) maxAge = bound
      c.maxAge = new FiniteDuration(maxAge, TimeUnit.SECONDS)
      c.path = path
      c.domain = domain
      c.isSecure = secure
      c.httpOnly = httpOnly
      if (version > 0) {
          c.comment = comment
      }
      if (version > 1) {
          c.commentUrl = commentURL
          c.ports = ports
          c.isDiscard = discard
      }

      cookies += c
      i += 1
    }

    return cookies
  }
}



object CookieHeaderNames {
  val PATH = "Path"
  val EXPIRES = "Expires"
  val MAX_AGE = "Max-Age"
  val DOMAIN = "Domain"
  val SECURE = "Secure"
  val HTTPONLY = "HTTPOnly"
  val COMMENT = "Comment"
  val COMMENTURL = "CommentURL"
  val DISCARD = "Discard"
  val PORT = "Port"
  val VERSION = "Version"
  val SAME_SITE = "SameSite"
}