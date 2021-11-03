name := "global_props"

version := "1.0.0"

organization := "net.aklabs"

crossScalaVersions := Seq("2.9.2", "2.12.3")

scalaVersion := "2.12.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-encoding", "UTF-8"
)

javacOptions ++= {
  Seq("-source", "1.5", "-target", "1.5", "-encoding", "UTF-8",
  "-Xlint:unchecked",
  "-Xlint:deprecation",
  "-Xms128m", "-Xmx1024m")
}


resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "Local Maven Repository" at "file:///C:/Users/AAndrievsky/.m2/repository"

//resolvers +=  "Twitter" at "http://maven.twttr.com"

//net.virtualvoid.sbt.graph.Plugin.graphSettings

libraryDependencies ++= Seq(
  "org.tinylog" % "tinylog" % "1.1",
  "commons-codec" % "commons-codec" % "1.9"
)

//scalacOptions ++= Seq("-deprecation", "-target:jvm-1.5")

//unmanagedJars in Compile += file("resources/lib/compiler_2.10.jar")

//jrebel.enabled := true

lazy val root = (project in file("."))//.dependsOn(jsonPatch).dependsOn(libcurlProj)
