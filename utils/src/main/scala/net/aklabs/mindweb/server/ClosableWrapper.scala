package net.aklabs.mindweb.server

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait ClosableWrapper {
	def close(after: FiniteDuration): Future[Unit]
}