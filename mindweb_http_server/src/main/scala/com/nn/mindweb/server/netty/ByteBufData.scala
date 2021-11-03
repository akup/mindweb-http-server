package com.nn.mindweb.server.netty

import io.netty.channel.ChannelHandlerContext
import net.aklabs.helpers.NonBlockingData

class ByteBufData(fileUpload: MemoryTransferFileUpload,
                  ctx: ChannelHandlerContext, contentLength: Long) extends NonBlockingData {
  private var readIndex = 0

  private var _released = false
  private var _readableBytes = 0
  private var lastRead: Array[Byte] = Array[Byte]()
  private def update(): Unit = {
    if (_readableBytes == 0) {
      lastRead = fileUpload.get()
      _readableBytes = lastRead.length
    }
  }
  override def release(): Unit = {
    if (!_released) {
      fileUpload.release()
      _released = true
    }
  }
  private def readableBytes(): Int = try {
    update()
    if (!ctx.channel().isOpen || _released) return -2
    if ((contentLength != 0 && readIndex >= contentLength) ||
      (_readableBytes == 0 && fileUpload.isCompleted)) return -1
    _readableBytes
  } catch {
    case e: Throwable => e.printStackTrace(); -2
  }
  def readAvailable(doneFunc: () => Unit,
                    breakFunc: () => Unit,
                    readFunc: (Array[Byte], Boolean) => Unit,
                    waitFunc: () => Unit): Unit = {
    val av = readableBytes()
    if (av == -1) {release(); doneFunc()}
    else if (av == -2) breakFunc()
    else if (av > 0) {
      readIndex += _readableBytes
      readFunc(lastRead, !_wasRead)
      markRead()
    } else waitFunc() //0
  }

  private var _wasRead = false
  private def markRead(): Unit = {
    _readableBytes = 0
    lastRead = Array[Byte]()
    _wasRead = true
  }
}
