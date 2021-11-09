package com.nn.mindweb.server

import com.nn.mindweb.server.MindwebServer.getServerProperty
import com.nn.mindweb.server.ServerContext._
import io.netty.handler.codec.http.{HttpHeaderNames, HttpResponseStatus}
import net.aklabs.helpers.Helpers
import org.apache.commons.io.FileUtils
import org.pmw.tinylog.Logger

import java.io.{File, RandomAccessFile}
import java.net.URLDecoder
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.ConcurrentHashMap
import javax.activation.MimetypesFileTypeMap
import scala.concurrent.Future

import netty._
import messages._

object FileService {
  private final val noDownload = Seq("text/html", "application/javascript", "text/css")

  private val minBytesToChunk =
    getServerProperty("fileservice_min_chunked").flatMap(m => Helpers.tryo{m.toLong}).getOrElse(0L)

  def getContentType(str: String): String = {
    extMap.getContentType(str)
  }

  def getContentType(file: File): String = {
    extMap.getContentType(file)
  }

  def init: String ={
    val start = System.currentTimeMillis()
    getContentType("1.txt") + lastModDate + expireDate  
  }

  lazy val extMap = new MimetypesFileTypeMap(
    FileService.getClass.getResourceAsStream("/META-INF/mime.types")
  )

  lazy val formatter = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz")
  lazy val lastModDate: String = formatter.format(new Date())
  lazy val expire: Date = {
    val dt = new Date()
    dt.setYear(dt.getYear+1)
    dt
  }
  lazy val expireDate: String = formatter.format(expire)
  
  val cache = new ConcurrentHashMap[String, (Long, Long, Array[Byte])]()
  val cacheExpired: Int = getServerProperty("fileservice_cache_expired").getOrElse("60000").toInt  //ms

  def getFileResponseInfo(file: File, cachePath: String, request: RemoteNettyHttpRequest): Option[Response] = {
    if(cache.containsKey(cachePath) || (file.exists && !file.isDirectory)){

      val t0 = System.currentTimeMillis()
      var (putTime, lastModified, arr) = Option(cache.get(cachePath)).getOrElse((0L, 0L, Array.empty[Byte]))

      val nettyRequest = request.request
      if (lastModified.toString != nettyRequest.headers().get(HttpHeaderNames.IF_NONE_MATCH)) {
        val (byteArray, resp_file) = if(t0 - putTime <= cacheExpired){
          Some(arr) -> None
        } else if(file.length() == arr.length && file.lastModified() == lastModified) {
          cache.put(cachePath, (t0, lastModified, arr))
          Some(arr) -> None
        } else {
          lastModified = file.lastModified()
          //cache.put(cachePath, (t0, file.lastModified(), b))

          if (minBytesToChunk > file.length()) {
            val arr = FileUtils.readFileToByteArray(file)
            Some(arr) -> None
          } else
            None -> Some(file)
          /*
          val b = FileUtils.readFileToByteArray(file)
          //cache.put(cachePath, (t0, file.lastModified(), b))
          b
           */
        }

        val fname = file.getName
        val ext = fname.split('.').last
  			val mtype = FileService.extMap.getContentType('.' + ext)

        val resp = Response(nettyRequest.protocolVersion(), HttpResponseStatus.OK)
        val contentLength: Long = byteArray.map(_.length.toLong).orElse(resp_file.map(_.length())).get
        resp.headers().add("Content-Type", mtype)
        resp.headers().add("Content-Length", contentLength.toString)
        resp.headers().add("Cache-Control", "private, max-age=60")
        resp.headers().add("Etag", lastModified.toString)
        resp.headers().add("Access-Control-Allow-Origin", "*")

        if (!noDownload.contains(mtype.toLowerCase())) {
          resp.headers().add("Content-Disposition", "attachment; filename=\"" + fname + "\"")
        }
        if (minBytesToChunk <= contentLength) {
          resp.setChunked(true)
          resp.headers().add("transfer-encoding", "chunked")
        }
        //resp.headers().add("connection", "chunked")

        byteArray match {
          case Some(arr) => resp.content = arr
          case _ => resp.file = Some(new RandomAccessFile(resp_file.get, "r"))
        }

        //TODO: add range

        Some(resp)
  		} else{
        Some(Response(nettyRequest.protocolVersion(), HttpResponseStatus.NOT_MODIFIED))
  		}
    } else None
  }
  
  def fileResponse(file: File, request: Request, filePath: Option[String]): Option[Future[Response]] = {
    getFileResponseInfo(file, filePath.getOrElse(request.path), request._request).map(response => {
      Future(response)
    })
  }
}

class FileService extends SimpleFilter[RemoteNettyHttpRequest, Response] {

  def apply(request: RemoteNettyHttpRequest, service: Service[RemoteNettyHttpRequest, Response]): Future[Response] = {
    val uri = URLDecoder.decode(request.request.uri(), "UTF-8")
    val f = if (uri != "/" && uri.nonEmpty ) {
      val filesFolderPath = getServerProperty("static_files_folder").getOrElse("resources/public")
      val q_ind = uri.indexOf("?")
      val path = if (q_ind > -1)
        request.request.uri().substring(0, q_ind)
      else
        request.request.uri()
      val file = new File(filesFolderPath, path)
      Logger.info(path + " " + file.getAbsolutePath + " : " + file.exists())
      FileService.getFileResponseInfo(file, path, request).map(response => {
        Future(response)
      }).getOrElse(service(request))
    } else service(request)
    val t_end = System.nanoTime
    
    f
  }
}