import sbt._

object MyBuild extends Build {
  lazy val root = Project(id = "root", base = file(".")) aggregate(processing2D)
  
  lazy val processing2D = Project(id = "processing2D", base = file("processing2D")) dependsOn(models, scalaProcessing)
  
  lazy val models = Project(id = "models", base = file("models"))  
  lazy val scalaProcessing = uri("git://github.com/alphaneet/scala-processing.git")
}
