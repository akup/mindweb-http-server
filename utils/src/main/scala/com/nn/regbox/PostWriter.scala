package com.nn.regbox

import java.util
import java.util.concurrent._

import org.pmw.tinylog.{Configuration, LogEntry}
import org.pmw.tinylog.writers.{LogEntryValue, VMShutdownHook, Writer}

class PostWriter extends Writer {

  val sb = new StringBuffer()
  val l = new ConcurrentLinkedQueue[String]()
  val executor = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable])
  val task = new pushTask

  override def close() = {
    VMShutdownHook.unregister(this)
    val text = sb.toString()
    sb.setLength(0)
    l.offer(text)
    executor.execute(task)
  }

  override def init(configuration: Configuration) = VMShutdownHook.register(this)
  override def getRequiredLogEntryValues = util.EnumSet.of(LogEntryValue.RENDERED_LOG_ENTRY)
  override def flush() = {}
  override def write(logEntry: LogEntry) = {
    //println("Post writer")
    sb.append(logEntry.getRenderedLogEntry())
    //println("Post writer: " + sb.length())
    if (sb.length() > 100000) {
      //println("Writing log to http")
      val text = sb.toString
      sb.setLength(0)
      l.offer(text)
      if(executor.getActiveCount == 0 )
        executor.execute(task)
    }
  }

  class pushTask extends Runnable {
    override def run():Unit = {
      BootstrapDI.bootstrapper.foreach(rbh => {
        var text = l.peek()
        var code = 200
        while (text != null && code == 200){
          //if (HttpClientUtils.isInited()) {
            code = HttpClientUtils.curlPostStr(
              rbh.getHostStr().append("reg_logs/" + rbh.getDeviceId.getOrElse("empty")).toString(),
              text).responseCode
            if (code == 200)
              l.poll()
          //} else l.poll()
          text = l.peek()
        }
      })
    }
  }
}
