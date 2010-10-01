import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
  val junit = "junit" % "junit" % "4.5" % "test->default"

  val scrip   = "com.razie" %% "scripster"       % "0.4-SNAPSHOT"

  override def unmanagedClasspath = 
    (Path.fromFile ("../razbase/lib") / "json.jar") +++
      super.unmanagedClasspath 
}

