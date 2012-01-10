import sbt._

object MyBuild extends Build {
  lazy val root = Project(id = "root", base = file(".")) aggregate(models)
  lazy val models = Project(id = "models", base = file("models")) 

}
