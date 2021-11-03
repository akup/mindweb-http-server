package com.aklabs.user

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode

import scala.collection.mutable
import scala.concurrent.Future


trait AbstractUser {
  def getId: Option[String]
  def getEntityUserId: Option[String]
  def getGroups: List[String]
  def isGroupF: Boolean
  def isUserF: Boolean
  def isSuperUser: Boolean

  def getMetaAccesses: List[(String, String)]
  def getSpMetaAccesses: List[(String, String, String)]
}

trait AbstractMetaUser {
  def loggedIn_? : Boolean
  def currentUser: Option[AbstractUser]
  def currentId: Option[String]
  def currentEntityId: Option[String]
  def logUserOut(): Unit
}


case class EntityUserId(entity: String, userId: String)
trait AbstractEdgeUser {
  def getUniqueId: Option[String]
  def getEntityId: Option[EntityUserId]
  def getEntityIdString: Option[String] = getEntityId.map(eu => "%s|%s".format(eu.entity, eu.userId))

  val edgeStore: mutable.Map[String, Any]
  def persistValue(key: String, value: ObjectNode): Option[Future[(Int, String, Seq[(String, JsonNode)])]]
  def getPersistantValue(key: String): Option[ObjectNode]
}

trait AbstractEdgeMetaUser {
  var onLogIn: List[(AbstractEdgeUser, String, String) => Unit] = Nil
  var onLogOut: List[(AbstractEdgeUser, String, String) => Unit] = Nil

  def loggedIn_? : Boolean
  def currentUser: Option[AbstractEdgeUser]
  def currentUserId: Option[String]
  def currentUserEntityId: Option[EntityUserId]
  def logUserOut(): Unit
  def loginByCookies(): Unit
}

/*
object DummyMetaUser extends AbstractMetaUser {
  override def loggedIn_? : Boolean = false
  override def currentUser: Option[AbstractUser] = None
  override def currentUserId: Option[String] = None
  override def logUserOut(): Unit = {}
  override def loginByCookies(): Unit = {}
}
object MetaUserHolder {
  private[user] var metaUser: AbstractMetaUser = DummyMetaUser

  def getMetaUser = metaUser
}
 */
