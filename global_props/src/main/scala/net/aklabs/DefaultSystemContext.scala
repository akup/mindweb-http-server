package net.aklabs

import akka.actor.ActorSystem
import akka.dispatch.MessageDispatcher

object DefaultSystemContext {
  val system: ActorSystem = ActorSystem("defsystem")
  implicit val executionContext: MessageDispatcher = system.dispatchers.lookup("akka.actor.default-dispatcher")
}
