import play.Project._

import bintray.Plugin._

import bintray.Keys._

bintraySettings

organization := "com.github.joprice"

name := "securesocial"

version := "2.1.3.3"

libraryDependencies ++= Seq(
  cache,
  "com.typesafe" %% "play-plugins-util" % "2.2.0",
  "com.typesafe" %% "play-plugins-mailer" % "2.2.0",
  "org.mindrot" % "jbcrypt" % "0.3m"
)

resolvers ++= Seq(
  Resolver.typesafeRepo("releases")
)

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))

scalacOptions := Seq("-feature", "-deprecation")

playScalaSettings

