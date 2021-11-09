package com.nn.mindweb.server
package netty

import messages._

import io.netty.channel.ChannelInitializer
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec.http.{HttpRequestDecoder, HttpResponseEncoder}
import io.netty.handler.stream.ChunkedWriteHandler

class ServerInitializer(service: Service[RemoteNettyHttpRequest, Response]) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit = {
    // Create a default pipeline implementation.
    val pipeline = ch.pipeline()

    pipeline.addLast("decoder", new HttpRequestDecoder())
    pipeline.addLast("encoder", new HttpResponseEncoder())
    pipeline.addLast("chunkedWriter", new ChunkedWriteHandler())
    //pipeline.addLast("deflater", new HttpContentCompressor(1));

    pipeline.addLast("handler", new HttpServerHandler(service))
  }
}