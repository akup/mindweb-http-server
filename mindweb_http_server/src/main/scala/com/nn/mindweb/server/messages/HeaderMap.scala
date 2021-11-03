package com.nn.mindweb.server.messages

import scala.collection.mutable


/**
 * Message headers map.
 *
 * Header names are case-insensitive.  For example, get("accept") is the same as
 * get("Accept").
 *
 * The map is a multi-map.  Use getAll() to get all values for a key.  Use add()
 * to append a key-value.
 */
abstract class HeaderMap
  extends mutable.Map[String, String]
  with mutable.MapLike[String, String, HeaderMap] {

  def getAll(key: String): Iterable[String]

  /** Add a header but don't replace existing header(s). */
  def add(k: String, v: String): HeaderMap

  override def empty: HeaderMap = new MapHeaderMap(mutable.Map.empty)
}


/** Mutable-Map-backed HeaderMap */
class MapHeaderMap(underlying: mutable.Map[String, Seq[String]]) extends HeaderMap {

  def getAll(key: String): Iterable[String] =
    underlying.getOrElse(key, Nil)

  def add(k: String, v: String): HeaderMap = {
    underlying(k) = underlying.getOrElse(k, Nil) :+ v
    this
  }

  // For Map/MapLike
  def get(key: String): Option[String] = {
    underlying.find { case (k, _) => k.equalsIgnoreCase(key) } flatMap { _._2.headOption }
  }

  // For Map/MapLike
  def iterator: Iterator[(String, String)] = {
    for ((k, vs) <- underlying.iterator; v <- vs) yield
      (k, v)
  }

  // For Map/MapLike
  def += (kv: (String, String)): MapHeaderMap.this.type = {
    underlying(kv._1) = Seq(kv._2)
    this
  }

  // For Map/MapLike
  def -= (key: String): MapHeaderMap.this.type = {
    underlying.retain { case (a, _) => !a.equalsIgnoreCase(key) }
    this
  }

  override def keys: Iterable[String] =
    underlying.keys

  override def keySet: Set[String] =
    underlying.keySet.toSet

  override def keysIterator: Iterator[String] =
    underlying.keysIterator
}


object MapHeaderMap {
  def apply(headers: (String, String)*): MapHeaderMap = {
    val map = headers
      .groupBy { case (k, _) => k.toLowerCase }
      .mapValues(_.map {_._2}) // remove keys
    new MapHeaderMap(mutable.Map() ++ map)
  }
}