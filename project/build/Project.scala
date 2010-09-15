import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
  val junit = "junit" % "junit" % "4.5" % "test->default"

  val razXml  = "com.razie" %% "razxml"          % "0.1-SNAPSHOT"
  val razBase = "com.razie" %% "razbase"         % "0.1-SNAPSHOT"
  val w20     = "com.razie" %% "20widgets"       % "0.1-SNAPSHOT"
  val w20s    = "com.razie" %% "20widgets-swing" % "0.1-SNAPSHOT"
  val razWeb  = "com.razie" %% "razweb"          % "0.1-SNAPSHOT"
  val scrip   = "com.razie" %% "scripster"       % "0.4-SNAPSHOT"

  override def unmanagedClasspath = 
    (Path.fromFile ("../razbase/lib") / "json.jar") +++
      super.unmanagedClasspath 
 
  override def mainScalaSourcePath = "src"
  override def testScalaSourcePath = "test_src"
}

