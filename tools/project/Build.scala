import sbt._

object MyBuild extends Build {
  lazy val root = Project(id = "root", base = file(".")) dependsOn(models, scalaProcessing)
  lazy val models = file("../models")
  lazy val scalaProcessing = uri("git://github.com/alphaneet/scala-processing.git")
}
