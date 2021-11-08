package com.nn.http

import java.net.URLEncoder

import scala.collection.immutable

/**
 * Request parameter map.
 *
 * This is a multi-map.  Use getAll() get all values for a key.
 */
abstract class ParamMap
  extends immutable.Map[String, String]
  with immutable.MapLike[String, String, ParamMap] {

  /**
   * Add a key/value pair to the map, returning a new map.
   * Overwrites all values if the key exists.
   */
  def +[B >: String](kv: (String, B)): ParamMap = {
    val (key, value) = (kv._1, kv._2.toString)
    val map = MapParamMap.tuplesToMultiMap(iterator.toSeq)
    val mapWithKey = map.updated(key, Seq(value))
    new MapParamMap(mapWithKey, isValid)
  }

  /**
   * Removes a key from this map, returning a new map.
   * All values for the key are removed.
   */
  def -(name: String): ParamMap = {
    val map = MapParamMap.tuplesToMultiMap(iterator.toSeq)
    new MapParamMap(map - name, isValid)
  }

  // For Map/MapLike
  override def empty: ParamMap =
    EmptyParamMap

  // For Map/MapLike (ensures keys aren't repeated)
  override def keySet: Set[String] =
    // super.keySet can actually have the same element multiple times
    super.keySet.toSeq.distinct.toSet

  // For Map/MapLike (ensures keys aren't repeated)
  override def keysIterator: Iterator[String] =
    super.keysIterator.toSeq.distinct.iterator

  /**
   * Check if there was a parse error.  On a parse error, the parameters
   * are treated as empty (versus throwing a parse exception).
   */
  def isValid: Boolean

  /** Get all parameters with name. */
  def getAll(name: String): Iterable[String]

  /* Equivalent to get(name).getOrElse(default). */
  def getOrElse(name: String, default: => String): String =
    get(name).getOrElse(default)

  /** Get Short value.  Uses forgiving StringUtil.toSomeShort to parse. */
  def getShort(name: String): Option[Short] =
    get(name) map { StringUtil.toSomeShort }

  /** Get Short value or default.  Equivalent to getShort(name).getOrElse(default). */
  def getShortOrElse(name: String, default: => Short): Short =
    getShort(name) getOrElse default

  /** Get Int value.  Uses forgiving StringUtil.toSomeInt to parse. */
  def getInt(name: String): Option[Int] =
    get(name) map { StringUtil.toSomeInt }

  /** Get Int value or default.  Equivalent to getInt(name).getOrElse(default). */
  def getIntOrElse(name: String, default: => Int): Int =
    getInt(name) getOrElse default

  /** Get Long value.  Uses forgiving StringUtil.toLong to parse. */
  def getLong(name: String): Option[Long] =
    get(name) map { StringUtil.toSomeLong }

  /** Get Long value or default.  Equivalent to getLong(name).getOrElse(default). */
  def getLongOrElse(name: String, default: => Long): Long =
    getLong(name) getOrElse default

  /** Get Boolean value.  True is "1" or "true", false is all other values. */
  def getBoolean(name: String): Option[Boolean] =
    get(name) map { _.toLowerCase } map { v => v == "1" || v == "t" || v == "true" }

  /** Get Boolean value or default. Equivalent to getBoolean(name).getOrElse(default). */
  def getBooleanOrElse(name: String, default: => Boolean): Boolean =
    getBoolean(name) getOrElse default

  override def toString: String = {
    val params = iterator.toList.map { kv =>
      URLEncoder.encode(kv._1, "utf-8") + "=" + URLEncoder.encode(kv._2, "utf-8")
    }
    if (params.nonEmpty) params.reduce(_ + "&" + _) else ""
  }
}


/** Map-backed ParamMap. */
class MapParamMap(
    underlying: Map[String, Seq[String]],
    val isValid: Boolean = true)
  extends ParamMap {

  def get(name: String): Option[String] =
    underlying.get(name) flatMap { _.headOption }

  def getAll(name: String): Iterable[String] =
    underlying.getOrElse(name, Nil)

  def iterator: Iterator[(String, String)] = {
    for ((k, vs) <- underlying.iterator; v <- vs) yield
      (k, v)
  }

  override def keySet: Set[String] =
    underlying.keySet

  override def keysIterator: Iterator[String] =
    underlying.keysIterator
}


object MapParamMap {
  def apply(params: (String, String)*): MapParamMap =
    new MapParamMap(MapParamMap.tuplesToMultiMap(params))

  def apply(map: Map[String, String]): MapParamMap =
    new MapParamMap(map.mapValues { value => Seq(value) })

  private[http] def tuplesToMultiMap(
      tuples: Seq[(String, String)]
  ): Map[String, Seq[String]] = {
    tuples
      .groupBy { case (k, _) => k }
      .mapValues(values => values.map {
        _._2
      })
  }
}


/** Empty ParamMap */
object EmptyParamMap extends ParamMap {
  val isValid = true
  def get(name: String): Option[String] = None
  def getAll(name: String): Iterable[String] = Nil
  def iterator: Iterator[(String, String)] = Iterator.empty
  override def -(name: String): ParamMap = this
}



private object StringUtil {

  private val SomeIntRegex = """\A\s*(-?\d+).*\Z""".r

  /**
   * Convert s to a Int liberally: initial whitespace and zeros are
   * skipped, non-digits after the number are ignored, and the default is 0.
   */
  def toSomeShort(s: String): Short = {
    SomeIntRegex.findFirstMatchIn(s) match {
      case Some(sMatch) =>
        try {
          sMatch.group(1).toShort
        } catch {
          case _: NumberFormatException => 0
        }
      case None =>
        0
    }
  }

  /**
   * Convert s to an Int liberally: initial whitespace and zeros are
   * skipped, non-digits after the number are ignored, and the default is 0.
   */
  def toSomeInt(s: String): Int = {
    SomeIntRegex.findFirstMatchIn(s) match {
      case Some(sMatch) =>
        try {
          sMatch.group(1).toInt
        } catch {
          case _: NumberFormatException => 0
        }
      case None =>
        0
    }
  }

  /**
   * Convert s to a Long liberally: initial whitespace and zeros are
   * skipped, non-digits after the number are ignored, and the default is 0L.
   */
  def toSomeLong(s: String): Long = {
    SomeIntRegex.findFirstMatchIn(s) match {
      case Some(sMatch) =>
        try {
          sMatch.group(1).toLong
        } catch {
          case _: NumberFormatException => 0L
        }
      case None =>
        0L
    }
  }
}