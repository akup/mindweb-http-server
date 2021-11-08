/*
 * Copyright 2007-2011 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.nn.http

import scala.collection.JavaConverters._
import java.util.concurrent.atomic.AtomicLong

import net.aklabs.helpers.JsonHelpers.{JObject, JValue}

import scala.collection.mutable
import scala.concurrent.Future

/**
 * The representation of a HTTP session
 */
class HttpSession(private val sess_id: String) {

  /**
   * @return - the HTTP session ID
   */
  def sessionId: String = sess_id


  private var inactiveInterval = 3 * 60 * 60 * 1000L //3 часа
  /**
   * @return - the maximim interval in seconds between client request and the time when
   *            the session will be terminated
   *
   */
  def maxInactiveInterval: Long = inactiveInterval

  /**
   * Sets the maximim interval in seconds between client request and the time when
   * the session will be terminated
   *
   * @param interval - the value in seconds
   *
   */
  def setMaxInactiveInterval(interval: Long): Unit = inactiveInterval = interval

  
  private val _lastAccessedTime: AtomicLong = new AtomicLong(System.currentTimeMillis())
  
  def touchSession(): Unit = _lastAccessedTime.set(System.currentTimeMillis())
  /**
   * @return - the last time server receivsd a client request for this session
   */
  def lastAccessedTime: Long = _lastAccessedTime.get()

  
  private val store = new java.util.concurrent.ConcurrentHashMap[String, Any]().asScala
  private val storedGlobal = new java.util.concurrent.ConcurrentHashMap[String, Any]().asScala

  var _onStoreChange: Option[(String, Option[String], String, String) => Unit] = None

  var _onPersistantAdd: Option[(String, JObject, Option[String], Option[String], Option[AnyRef]) => Future[(Int, String, Seq[(String, JValue)])]] = None
  
  /**
   * Sets a value associated with a name for this session
   *
   * @param name - the attribute name
   * @param value - any value
   */
  def setAttribute(name: String, value: Any): Option[Any] = {
    touchSession()
    store.put(name, value)
  }

  def setAttributeGlobal(name: String, value: String,
                         ip: String = "", userAgent: String = ""): Unit = {
    touchSession()
    store.put(name, value)
    storedGlobal.put(name, true)
    _onStoreChange.foreach(_(name, Some(value), ip, userAgent))
  }
  def getGlobalAttributes(): Seq[(String, Option[String])] = {
    storedGlobal.keys.toSeq.flatMap(key => store.get(key).map(x => key -> Some(x.toString)))
  }

  def setPersistant(key: String, obj: JObject, user: Option[AnyRef] = None): Option[Future[(Int, String, Seq[(String, JValue)])]] = {
    touchSession()
    val ip = R.request.map(_.remoteAddress.toString)
    val userAgent = R.request.flatMap(_.getHeader("User-Agent"))
    _onPersistantAdd.map(_(key, obj, ip, userAgent, user))
  }

  def synchronizeOnStore[T](f: => T): T = store.synchronized{
    f
  }
  /**
   * @param name - the attribute name
   * @return - the attribute value associated with this name
   */
  def attribute(name: String): Option[Any] = {
    touchSession()
    store.get(name)
  }

  /**
   * Removes the session attribute having this name
   *
   * @param name - the attribute name
   */
  def removeAttribute(name: String, ip: String = "", userAgent: String = ""): Unit = {
    touchSession()
    store.remove(name).foreach(v => {
      if (storedGlobal.contains(name)) {
        storedGlobal.remove(name)
        _onStoreChange.foreach(_(name, None, ip, userAgent))
      }
    })
  }

  /**
   * Terminates this session
   */
  def terminate(): Unit = {
    store.clear()
    _onTerminate.foreach(_())
  }
  var _onTerminate: Option[() => Unit] = None
  
  
  
  private var _theSession: Option[TheSession] = None
  /**
   * Links a LiftSession with this HTTP session. Hence when the HTTP session
   * terminates or times out LiftSession will be destroyed as well.
   *
   * @param theSession - the LiftSession
   */
  def link(theSession: TheSession): Unit = this.synchronized {
    if (_theSession.isDefined) throw new Exception("Other session is already linked")
    _theSession = Some(theSession)
    touchSession()
  }

  /**
   * The opposite of the <i>link</i>. Hence the LiftSession and the HTTP session no
   * longer needs to be related. It is called when LiftSession is explicitelly terminated.
   *
   * @param theSession - the Session
   */
  def unlink(theSession: TheSession): Unit = this.synchronized {
    _theSession = None
    terminate()
  }
  
  def linkedSuperSession: Option[TheSession] = _theSession
}

