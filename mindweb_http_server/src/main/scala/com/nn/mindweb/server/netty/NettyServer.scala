package com.nn.mindweb.server
package netty

import com.nn.mindweb.server.messages.Response
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.channel.{ChannelFuture, EventLoopGroup}

import scala.concurrent.Future
import org.pmw.tinylog.Logger

class NettyServer {
  private var bossWorkerGroup: Option[(EventLoopGroup, EventLoopGroup)] = None
  private var nettyBossFuture: Option[ChannelFuture] = None

  def serve(port: Int, service: Service[RemoteNettyHttpRequest, Response]): Unit = {
    Logger.info("try to bind to " + port)
    val b = new ServerBootstrap
    bossWorkerGroup = Some(new NioEventLoopGroup, new NioEventLoopGroup)
    b.group(bossWorkerGroup.get._1, bossWorkerGroup.get._2)
      .channel(classOf[NioServerSocketChannel])
      .childHandler(new ServerInitializer(service))
    nettyBossFuture = Some(b.bind(port).sync)

    Future {
      try {
        Logger.info("netty started")
        nettyBossFuture.get.channel.closeFuture.sync // close port
        Logger.info("netty stopped")
      } finally {
        bossWorkerGroup.foreach(groups => {
          groups._1.shutdownGracefully.sync
          groups._2.shutdownGracefully.sync
        })
      }
      Logger.debug("netty future done")
    }(ServerContext.task_dispatcher)
  }

  def stop(): Unit = {
    Logger.info("stopping netty")
    try { // shutdown EventLoopGroup
      bossWorkerGroup.foreach(groups => {
        bossWorkerGroup = None
        groups._1.shutdownGracefully.sync
        groups._2.shutdownGracefully.sync
      })
      Logger.info("netty resources released")
      //nettyBossFuture.foreach(_.channel.closeFuture.sync) // close port
    } catch {
      case e: InterruptedException =>
        e.printStackTrace()
    }
  }
}