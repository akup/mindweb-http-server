package com.nn.utils

import org.pmw.tinylog.Logger

import sys.process._
/**
  * Created by Ivan on 21.04.2017.
  */
object HostInfo {

  def get() = {
//    Darwin MacBook-AK.local 16.5.0 Darwin Kernel Version 16.5.0: Fri Mar  3 16:52:33 PST 2017; root:xnu-3789.51.2~3/RELEASE_X86_64 x86_64
//    Linux ubuntu 4.8.0-41-generic #44-Ubuntu SMP Fri Mar 3 15:27:17 UTC 2017 x86_64 x86_64 x86_64 GNU/Linux
//    Linux raspberrypi 4.4.32-v7+ #924 SMP Tue Nov 15 18:11:28 GMT 2016 armv7l GNU/Linux
//    Linux OpenWrt ......

//    для ос: linux, macosx, openwrt,
//    для архитектур: x86, x64, arm
    val res = "uname -a" !!

    var arch = res.toLowerCase match {
      case s if s.contains("x86_64")                     => "x64"
      case s if s.contains("i386") || s.contains("i686") => "x86"
      case s if s.contains("arm")                        => "arm"
      case _ => Logger.error("unknown: {}", res); null
    }

    var os = res.toLowerCase match {
      case s if s.contains("openwrt") => "openwrt"
      case s if s.contains("darwin")  => "macosx"
      case s if s.contains("linux")   => "linux"
      case _ => Logger.error("unknown: {}", res); null
    }

    HostInfo(Option(arch), Option(os))
  }

}

case class HostInfo(architecture:Option[String], operationSystem:Option[String])