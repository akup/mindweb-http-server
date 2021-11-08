name := "reg_utils"

version := "1.1.0"

scalaVersion := "2.12.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "Local Maven Repository" at "file:///C:/Users/AAndrievsky/.m2/repository"

//resolvers +=  "Twitter" at "http://maven.twttr.com"

libraryDependencies ++= Seq(
  //"net.aklabs" %% "global_props" % "1.0.0",
  "com.google.code.findbugs" % "jsr305" % "3.0.2",
  "commons-codec" % "commons-codec" % "1.9",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.11.0",
  "com.typesafe.akka" %% "akka-actor-typed" % "2.6.5",
  "com.github.spullara.mustache.java" % "scala-extensions-2.12" % "0.9.6",
  "com.github.spullara.mustache.java" % "compiler" % "0.9.6",
  "org.tinylog" % "tinylog" % "1.1",

  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

scalacOptions ++= Seq("-deprecation")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

//unmanagedJars in Compile += file("resources/lib/compiler_2.10.jar")
EclipseKeys.withSource := true

lazy val jsonPatch = RootProject(file("../json-patch"))
lazy val libcurlProj = RootProject(file("../libcurl"))
lazy val globpropsProj = RootProject(file("../global_props"))

lazy val root = (project in file(".")).dependsOn(jsonPatch).dependsOn(libcurlProj).dependsOn(globpropsProj)

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