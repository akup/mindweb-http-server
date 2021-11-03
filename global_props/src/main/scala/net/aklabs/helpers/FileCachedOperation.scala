package net.aklabs.helpers

import java.io.File

import scala.concurrent.Future
import java.util.concurrent.locks.{Lock, ReentrantLock}

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit._

import net.aklabs.DefaultSystemContext._
import org.pmw.tinylog.Logger

object FileCachedOperation {
  private var locks: Map[String, Lock] = Map.empty
  private var future: Option[Future[Unit]] = None

  def operate[T](getFile: () => Option[File],
                 readOperation: File => T, operation: (T, Option[File]) => Unit, opType: String): Unit = this.synchronized{
    val lock = locks.getOrElse(opType, {
      val lock = new ReentrantLock()
      locks += opType -> lock
      lock
    })
    lock.lock()
    if (future.isEmpty) {
      future = Some(Future {
        try {
          var file = getFile()
          while (file.isDefined) {
            operation(readOperation(file.get), file)
            file = getFile()
          }
        } catch {
          case e: Throwable =>
            Logger.error(e)
            //e.printStackTrace()
            //throw e
            system.scheduler.scheduleOnce(Duration(10, SECONDS)){
              operate(getFile, readOperation, operation, opType)
            }
        }
      })
      future.get.onComplete(_ => {
        lock.lock()
        future = None
        lock.unlock()
      })
    }
    lock.unlock()
  }
}
