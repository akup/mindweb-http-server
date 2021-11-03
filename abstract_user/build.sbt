name := "abstract_user"

version := "2.0.0"

lazy val buildSettings = Seq(
  organization := "com.aklabs",
  scalaVersion := "2.12.3"
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-unused:imports",
  "-encoding", "UTF-8"
)

lazy val commonJavacOptions = Seq(
  "-Xlint:unchecked",
  "-Xlint:deprecation"
)

lazy val commonSettings = Seq(
  Compile / scalacOptions ++= commonScalacOptions,
  Compile / javacOptions ++= commonJavacOptions,
  run / javaOptions ++= Seq("-Xms128m", "-Xmx1024m"),
  run / fork := false,
  Global / cancelable := false,
  licenses := Seq(
    ("CC0", url("http://creativecommons.org/publicdomain/zero/1.0"))
  )
)


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "Local Maven Repository" at "file:///C:/Users/AAndrievsky/.m2/repository"

//resolvers +=  "Twitter" at "http://maven.twttr.com"

//net.virtualvoid.sbt.graph.Plugin.graphSettings

libraryDependencies ++= Seq(
  "org.tinylog" % "tinylog" % "1.1",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.11.0",
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

//scalacOptions ++= Seq("-deprecation", "-target:jvm-1.5")

//unmanagedJars in Compile += file("resources/lib/compiler_2.10.jar")

//jrebel.enabled := true

lazy val root = (project in file(".")).settings(commonSettings)

/*
javacOptions ++= {
	Seq("-source", "1.5", "-target", "1.5", "-encoding", "UTF-8")
}
*/