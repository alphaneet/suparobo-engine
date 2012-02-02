import sbt._
import Keys._

object MyBuild extends Build {
  val branchName = "git branch".lines_!.find{_.head == '*'}.map{_.drop(2)}.getOrElse("") + "> "
  
  lazy val root = Project(
    id = "tools",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq( shellPrompt := { _ => branchName } )
  ) dependsOn(models, scalaProcessing)
  
  lazy val models = file("../models")
  
//  lazy val scalaProcessing = uri("git://github.com/alphaneet/scala-processing.git")

  lazy val scalaProcessing = file("../scala-processing")  
}
