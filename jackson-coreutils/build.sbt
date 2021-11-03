import sbt._
import Process._

name := "jackson-coreutils"

organization := "com.nn"

version := "1.9"

crossScalaVersions in ThisBuild    := Seq("2.9.2")

scalaVersion := "2.12.3"

resolvers += "Typesafe Repository" at "http://typesafe.artifactoryonline.com/typesafe/repo"

EclipseKeys.withSource := true

libraryDependencies ++= Seq(
  "com.google.code.findbugs" % "jsr305" % "2.0.1",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.3",
  "com.google.guava" % "guava" % "16.0.1",
  "com.github.fge" % "msg-simple" % "1.1",

  "org.mockito" % "mockito-all" % "1.10.19" % Test,
  "org.testng" % "testng" % "6.8.7" % Test
)

scalacOptions ++= Seq("-deprecation")

credentials in ThisBuild += Credentials(
     "Repository Archiva Managed internal Repository",
          "repo.nexpo.me",
          "admin",
          "archivaPSW99"
        )

publishTo in ThisBuild := Some("internal" at "http://repo.nexpo.me/repository/internal/")
