name := "mindweb_http_server"

version := "2.0.0"

scalaVersion := "2.12.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers +=  "Twitter" at "http://maven.twttr.com"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

libraryDependencies ++= Seq(
  "io.netty" % "netty-transport" % "4.1.48.Final",
  "io.netty" % "netty-buffer" % "4.1.48.Final",
  "io.netty" % "netty-handler" % "4.1.48.Final",
  "io.netty" % "netty-codec-http" % "4.1.48.Final",
  "io.netty" % "netty-common" % "4.1.48.Final",


  //For sessions grpc client and server
  "io.grpc" % "grpc-core" % "1.30.2",
  "io.grpc" % "grpc-api" % "1.30.2",
  "io.grpc" % "grpc-netty" % "1.30.2",

  "com.sun.activation" % "javax.activation" % "1.2.0",

  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.5",

  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

scalacOptions ++= Seq("-deprecation")

//unmanagedJars in Compile += file("resources/lib/compiler_2.10.jar")

EclipseKeys.withSource := true

lazy val regUtils = RootProject(file("../utils"))
lazy val htmlParser = RootProject(file("../parser/libmodest"))
lazy val edge_user = RootProject(file("../login_service/login_edge"))
lazy val session_protocol = RootProject(file("../session_service/session_protocol"))
lazy val db_protocol = RootProject(file("../db_api/db_protocol"))
lazy val root = (project in file("."))
    .dependsOn(regUtils)
    .dependsOn(htmlParser)
    .dependsOn(edge_user)
    .dependsOn(session_protocol)
    .dependsOn(db_protocol)

//Создаем класс содержащий информацию о версии сборки, чтобы не лазить в манифест
sourceGenerators in Compile += Def.task{
  val d = (sourceManaged in Compile).value
  val v = version.value
  val n = name.value
  val file = d / "info.scala"
  IO.write(file, """package com.nn
                   |object Info {
                   |  val version = "%s"
                   |  val name = "%s"
                   |}
                   |""".stripMargin.format(v, n))
  Seq(file)
}