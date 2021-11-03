name := "json-patch"

version := "1.9"

scalaVersion := "2.12.3"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Local Maven Repository" at "file:///C:/Users/AAndrievsky/.m2/repository"

resolvers +=  "Twitter" at "http://maven.twttr.com"

libraryDependencies ++= Seq(
  "com.google.code.findbugs" % "jsr305" % "2.0.1",

  "org.mockito" % "mockito-all" % "1.10.19" % Test,
  "org.assertj" % "assertj-core" % "3.16.1" % Test,
  "org.testng" % "testng" % "6.8.7" % Test
)

scalacOptions ++= Seq("-deprecation")

//unmanagedJars in Compile += file("resources/lib/compiler_2.10.jar")

EclipseKeys.withSource := true

lazy val jacksonCoreutils = RootProject(file("../jackson-coreutils"))

lazy val root = (project in file(".")).dependsOn(jacksonCoreutils)
