package com.nn
package http

import java.net.{InetAddress, InetSocketAddress}
import java.util
import java.util.{List => JList, Map => JMap, Set => JSet}

import net.aklabs.helpers.{FileParamHolder, RichThreadLocal}

import scala.collection.JavaConverters._

trait HttpRequest {
  /**
	 * Returns the method name (string) of request.
	 */
	def getMethodName: String
	/**
	 * Returns the URI (or path) of this request.
	 */
	def getUri: String
	
	def getHeader(name: String): Option[String]
	def getSession: Option[HttpSession] = session
	
	
	def params: ParamMap
  def files: Map[String, FileParamHolder]
  def isComplete: Boolean = true

  def methodName: String           = getMethodName
  //def method_=(method: HttpMethod) = setMethod(method)
  def uri: String                  = getUri
  
  val stateless_? : Boolean
  val session: Option[HttpSession]
	
	
	def snapshot: HttpRequest

  /** Path from URI. */
  def path: String = {
    val u = getUri
    u.indexOf('?') match {
      case -1 => u
      case n  => u.substring(0, n)
    }
  }

  /** File extension.  Empty string if none. */
  def fileExtension: String = {
    val p = path
    val leaf = p.lastIndexOf('/') match {
      case -1 => p
      case n  => p.substring(n + 1)
    }
    leaf.lastIndexOf('.') match {
      case -1 => ""
      case n  => leaf.substring(n + 1).toLowerCase
    }
  }

  /** Remote InetSocketAddress */
  def remoteSocketAddress: InetSocketAddress

  /** Remote host - a dotted quad */
  def remoteHost: String =
    remoteAddress.getHostAddress

  /** Remote InetAddress */
  def remoteAddress: InetAddress =
    remoteSocketAddress.getAddress

  /** Remote port */
  def remotePort: Int =
    remoteSocketAddress.getPort

  // The get*Param methods below are for Java compatibility.  Note Scala default
  // arguments aren't compatible with Java, so we need two versions of each.

  /** Get parameter value.  Returns value or null. */
  def getParam(name: String): String =
    params.get(name).orNull

  /** Get parameter value.  Returns value or default. */
  def getParam(name: String, default: String): String =
    params.getOrElse(name, default)

  /** Get Short param.  Returns value or 0. */
  def getShortParam(name: String): Short =
    params.getShortOrElse(name, 0)

  /** Get Short param.  Returns value or default. */
  def getShortParam(name: String, default: Short): Short =
    params.getShortOrElse(name, default)

  /** Get Int param.  Returns value or 0. */
  def getIntParam(name: String): Int =
    params.getIntOrElse(name, 0)

  /** Get Int param.  Returns value or default. */
  def getIntParam(name: String, default: Int): Int =
    params.getIntOrElse(name, default)

  /** Get Long param.  Returns value or 0. */
  def getLongParam(name: String): Long =
    params.getLongOrElse(name, 0L)

  /** Get Long param.  Returns value or default. */
  def getLongParam(name: String, default: Long=0L): Long =
    params.getLongOrElse(name, default)

  /** Get Boolean param.  Returns value or false. */
  def getBooleanParam(name: String): Boolean =
    params.getBooleanOrElse(name, default = false)

  /** Get Boolean param.  Returns value or default. */
  def getBooleanParam(name: String, default: Boolean): Boolean =
    params.getBooleanOrElse(name, default)

  /** Get all values of parameter.  Returns list of values. */
  def getParams(name: String): JList[String] =
    params.getAll(name).toList.asJava

  /** Get all parameters. */
  def getParams: JList[JMap.Entry[String, String]] =
    params.toList.map { case (k, v) =>
      // cast to appease asJava
      new util.AbstractMap.SimpleImmutableEntry(k, v).asInstanceOf[JMap.Entry[String, String]]
    }.asJava

  /** Check if parameter exists. */
  def containsParam(name: String): Boolean =
    params.contains(name)

  /** Get parameters names. */
  def getParamNames: JSet[String] =
    params.keySet.asJava

  def paramNames: Seq[String] = getParamNames.asScala.toSeq
}

object HttpRequest {
  /**
   * Create a nil request... useful for testing
   */
  def nil(sess: HttpSession): HttpRequest = nil(Some(sess))
  def nil(sess: Option[HttpSession] = None): HttpRequest = new HttpRequest() {
    override def getMethodName = "GET"
    override def getUri = ""
    override def getHeader(name: String): Option[String] = None
    override val session: Option[HttpSession] = sess
    override def params: ParamMap = EmptyParamMap
    override def files: Map[String, FileParamHolder] = Map.empty
    override def remoteSocketAddress = new InetSocketAddress(80)
    override val stateless_? : Boolean = false
    
    override def snapshot: HttpRequest = {
      val self = this
      new HttpRequest() {
		    override def getMethodName = "GET"
		    override def getUri = ""
		    override def getHeader(name: String): Option[String] = None
		    override val session: Option[HttpSession] = sess
		    override def params: ParamMap = EmptyParamMap
        override def files: Map[String, FileParamHolder] = Map.empty

		    override def remoteSocketAddress = new InetSocketAddress(80)
		    override val stateless_? : Boolean = false
		    
		    override def snapshot: HttpRequest = self.snapshot
      }
    }
  }
}

object CurrentReq extends RichThreadLocal[HttpRequest]