package com.nn.regbox.server.netty

import com.nn.mindweb.server.messages.Response
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToByteEncoder
import io.netty.util.CharsetUtil
import org.pmw.tinylog.Logger
/*
class ResponseEncoder extends MessageToByteEncoder[Response] {
  override protected def encode(ctx: ChannelHandlerContext, response: Response, out: ByteBuf): Unit = {
    Logger.debug("Call encode response")
    val httpHead = response.httpHead
    out.writeBytes(Unpooled.copiedBuffer(httpHead, CharsetUtil.UTF_8))
    if (!response.isChunked())
      out.writeBytes(response.content)
  }
}
*/