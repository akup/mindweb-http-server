package com.nn.mindweb.server
package netty

import com.nn.mindweb.server.messages.Response

import java.io.ByteArrayInputStream
import java.net.InetSocketAddress
import io.netty.buffer.Unpooled
import io.netty.channel.{ChannelDuplexHandler, ChannelFutureListener, ChannelHandlerContext}
import io.netty.handler.codec.http.multipart.HttpPostRequestDecoder.EndOfDataDecoderException
import io.netty.handler.codec.http.multipart.{Attribute, HttpPostRequestDecoder}
import io.netty.handler.codec.http.{HttpContent, HttpHeaderNames, HttpMethod, HttpObject, HttpResponseStatus, HttpVersion, LastHttpContent, HttpRequest => NettyHttpRequest}
import io.netty.handler.stream.{ChunkedFile, ChunkedStream}
import io.netty.util.{CharsetUtil, ReferenceCountUtil}
import net.aklabs.helpers.NonBlockingData
import org.pmw.tinylog.Logger

import scala.collection.mutable
import scala.util.{Failure, Success}


case class FileInfo(fileName: String, mimeType: String, length: Long, data: NonBlockingData)
case class RemoteNettyHttpRequest(request: NettyHttpRequest,
                                  _remoteAddress: InetSocketAddress = new InetSocketAddress(0),
                                  var _addPostParam: Option[(String, String) => Unit] = None,
                                  var _addFile: Option[(String, () => FileInfo) => Unit] = None,
                                  var _completeFile: Option[String => Unit] = None,
                                  var isComplete: Boolean = false) {
  private var postParams: Map[String, String] = Map.empty
  def addPostParam(name: String, value: String): Unit = _addPostParam match {
    case Some(func) => func(name, value)
    case _ => postParams += name -> value
  }
  def getPostParams: Map[String, String] = postParams

  private var files: Map[String, (FileInfo, Boolean)] = Map.empty
  def addFile(name: String, is: () => FileInfo): Unit = _addFile match {
    case Some(func) => func(name, is)
    case _ => if (!files.isDefinedAt(name)) files += name -> (is(), false)
  }
  def completeFile(name: String): Unit = _completeFile match {
    case Some(func) => func(name)
    case _ => files.get(name).foreach(v => {
      files += name -> (v._1, true)
    })
  }
  def getFiles: Map[String, (FileInfo, Boolean)] = files

}

class HttpServerHandler(service: Service[RemoteNettyHttpRequest, Response]) extends ChannelDuplexHandler {

  @throws[Exception]
  override def channelRead(ctx: ChannelHandlerContext, msg: Any): Unit = {
    //Logger.debug("channelRead: " + msg)
    var release = true
    try {
      msg match {
        case httpObj: HttpObject => channelReadObject(ctx, httpObj)
        case _ =>
          release = false
          ctx.fireChannelRead(msg)
      }
    }
    finally if (release) ReferenceCountUtil.release(msg)
  }

  //128kb in memory
  //private final val factory = new DefaultHttpDataFactory(0x20000)
  // все файлы храним в памяти! по этому нужно успеать их читать, чтобы память чистить.
  // При чтении нужно использовать discardReadBytes
  private final val factory = new HttpPostDataFactory(false)
  private var httpRequest: Option[RemoteNettyHttpRequest] = None
  private var httpDecoder: Option[HttpPostRequestDecoder] = None
  private val attrsToRelease: mutable.Map[String, Attribute] = mutable.Map.empty
  private var serviceStarted: Boolean = false
  private var postLength: Long = 0

  private def doService(ctx: ChannelHandlerContext, request: RemoteNettyHttpRequest): Unit = {
    Logger.debug("doService")
    serviceStarted = true
    service(request).onComplete {
      case Failure(exception) =>
        httpDecoder.foreach(_.destroy())
        httpDecoder = None
        attrsToRelease.clear()
        BadResponses.sendResponse(ctx, HttpResponseStatus.INTERNAL_SERVER_ERROR, exception.getMessage)
      case Success(response) =>
        httpDecoder.foreach(_.destroy())
        httpDecoder = None
        attrsToRelease.clear()

        response.headers().add("Server", "MindWeb Edge/2021-02-02")
        if (!response.isChunked) {
          response.headers().add("Connection", "close")
          ctx.write(response)
          ctx.writeAndFlush(Unpooled.copiedBuffer(response.content))
            .addListener(ChannelFutureListener.CLOSE)
          Logger.debug("End process request: " + System.nanoTime())
        }
        else {
          ctx.write(response)
          response.file match {
            case Some(f) =>
              Logger.debug("respond with chunked file")
              ctx.writeAndFlush(new ChunkedFile(f))
                .addListener(ChannelFutureListener.CLOSE)
            case _ =>
              Logger.debug("respond with chunked stream")
              ctx.writeAndFlush(new ChunkedStream(new ByteArrayInputStream(response.content)))
                .addListener(ChannelFutureListener.CLOSE)
          }
        }
      case _ =>
    }(ServerContext.flow_dispatcher)
  }

  private def channelReadObject(ctx: ChannelHandlerContext, httpObj: HttpObject): Unit = {
    httpObj match {
      case req: NettyHttpRequest =>
        Logger.debug("Start process request: " + System.nanoTime())
        httpRequest = Some(RemoteNettyHttpRequest(req, ctx.channel().remoteAddress().asInstanceOf[InetSocketAddress]))

        if (req.method == HttpMethod.POST) {
          httpDecoder = Some(new HttpPostRequestDecoder(factory, req))
          httpDecoder.get.setDiscardThreshold(0)
        } else if (req.method == HttpMethod.GET) {
          httpRequest.foreach(_.isComplete = true)
          doService(ctx, httpRequest.get)
        } else {
          BadResponses.sendResponse(ctx, HttpResponseStatus.METHOD_NOT_ALLOWED, null)
        }
      case _: LastHttpContent if httpDecoder.isEmpty =>
      case chunk: HttpContent => httpDecoder match {
        case Some(decoder) => try {
          //Logger.debug("Channel read decode: " + httpObj + " : " + chunk)
          postLength += chunk.content().readableBytes()
          if (postLength > MindwebServer.maxRequest)
            BadResponses.sendResponse(ctx, HttpResponseStatus.BAD_REQUEST,
              "Exceeds request size limit (%d bytes)".format(MindwebServer.maxRequest))
          else {
            //Logger.debug("Try offer chunk: " + httpObj)
            decoder.offer(chunk)
            //Logger.debug("AFTER DECODE" + httpObj)
            readMultipartChunk(ctx, httpRequest.get, attrsToRelease)
            /*
            if (decoder.isMultipart) {
              decoder.offer(chunk)
              readMultipartChunk(ctx, httpRequest.get)
            } else {
              val it = decoder.getBodyHttpDatas.iterator()
              while (it.hasNext) {
                it.next() match {
                  case attr: Attribute if attr.isCompleted =>
                    httpRequest.get.synchronized {
                      httpRequest.get.addPostParam(attr.getName, attr.getValue)
                    }
                    Logger.debug("the attribute: " + attr)
                    attr.release()
                }
              }
            }
             */
            //decoder.offer(chunk)
            //Logger.debug("Offered chunk")
            //readChunk(ctx, httpRequest.get)
            //Logger.debug("chunks read: " + chunk)

            //io.netty.util.internal.ReferenceCountUpdater

            if (chunk.isInstanceOf[LastHttpContent]) {
              //Logger.debug("attrsToRelease: " + attrsToRelease.values.toSet)
              attrsToRelease.values.foreach(a => try {
                a.release()
              } catch {
                case e: Throwable => e.printStackTrace()
              })
              if (!serviceStarted)
                doService(ctx, httpRequest.get)
              //Logger.debug("chunk reading is done")
              httpRequest.foreach(_.isComplete = true)
              httpDecoder = None
            }
          }
        } catch {
          case e: Throwable =>
            //e.printStackTrace()
            //игнорируем, могли уже закрыть канал, вернув ответ, до чтения всех данных.
        }
        case _ =>
          Logger.debug("discard")
          //chunk.release()
      }
      case x =>
        //Logger.debug("Bad http chunk: " + x)
        BadResponses.sendResponse(ctx, HttpResponseStatus.BAD_REQUEST, null)
    }
  }

  def readMultipartChunk(ctx: ChannelHandlerContext, request: RemoteNettyHttpRequest,
                         attrsToRelease: mutable.Map[String, Attribute]): Unit = {
    //Logger.debug("readChunk")
    val decoder = httpDecoder.get
    do {
      //Logger.debug("before next partial")
      val nextPartial = if (try {
        decoder.hasNext
      } catch {case _: EndOfDataDecoderException => false}) {decoder.next()}
      else decoder.currentPartialHttpData()

      //Logger.debug("nextPartial: " + nextPartial)

      nextPartial match {
        case attr: Attribute if attr.isCompleted =>
          request.synchronized{
            request.addPostParam(attr.getName, attr.getValue)
          }
          //Logger.debug("the attribute: " + attr)
          //Logger.debug("post param: " + request.getPostParams.get(attr.getName))

          attrsToRelease += attr.getName -> attr
          /*
          try {
            attr.release()
          } catch {
            case e: Throwable => e.printStackTrace()
          }
           */
        case fileUpload: MemoryTransferFileUpload =>
          if (MindwebServer.disableMultipartFiles)
            BadResponses.sendResponse(ctx, HttpResponseStatus.BAD_REQUEST, "File upload is disabled")
          else {
            request.synchronized {
              request.addFile(fileUpload.getName,
                () => FileInfo(fileUpload.getFilename, fileUpload.getContentType,
                  fileUpload.definedLength(), new ByteBufData(fileUpload, ctx, fileUpload.definedLength()))
              )
              if (fileUpload.isCompleted) {
                request.completeFile(fileUpload.getName)
              }
            }

            if (!serviceStarted)
              doService(ctx, httpRequest.get)
          } // finally if (fileUpload.isCompleted) fileUpload.release
        case x => Logger.debug("Unknown post part: " + x)
      }
      //Logger.debug("End of in-cycle operation")
    } while (try {
      decoder.hasNext
    } catch {case _: EndOfDataDecoderException => false})

/*
    while (decoder.hasNext) {
      decoder.next match {
        case attr: Attribute =>
          Logger.debug("the attribute: " + attr)
        case fileUpload: FileUpload => try {
          Logger.debug("fileUpload: " + fileUpload)
          fileUpload.getName
          fileUpload.getFilename
          fileUpload.get()
        } finally fileUpload.release
        case x => Logger.debug("Unknown post part: " + x)
      }
    }

 */
  }
}

object BadResponses {
  private[netty] def sendResponse(ctx: ChannelHandlerContext, status: HttpResponseStatus, message: String): Unit = {
    val sb = new StringBuilder
    if (message != null) sb.append(message)
    else sb.append("Failure: ").append(status.toString)
    sb.append(" \r\n")

    val msgDesc = sb.toString

    Logger.debug("Status: " + status)

    val response = Response(HttpVersion.HTTP_1_1, status)
    val content = msgDesc.getBytes(CharsetUtil.UTF_8)
    response.headers().add("Content-Length", content.length.toString)
    response.headers().add(HttpHeaderNames.CONTENT_TYPE.toString, "text/plain; charset=UTF-8")

    // Close the connection as soon as the response is sent.
    ctx.write(response)
    ctx.writeAndFlush(Unpooled.copiedBuffer(content))
      .addListener(ChannelFutureListener.CLOSE)
  }
}
