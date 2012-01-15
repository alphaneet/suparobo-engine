import sbt._
import Keys._

object ModelsBuild extends Build {
  val branchName = "git branch".lines_!.find{_.head == '*'}.map{_.drop(2)}.getOrElse("") + "> "
  
  lazy val root = Project(
    id = "models",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq( shellPrompt := { _ => branchName } )
  )
}
