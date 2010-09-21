import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) {
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
  val junit = "junit" % "junit" % "4.5" % "test->default"

  val razBase = "com.razie" % "razbase_2.8.0"         % "0.1-SNAPSHOT"
  val razXml  = "com.razie" % "razxml_2.8.0"          % "0.1-SNAPSHOT"
  val w20     = "com.razie" % "20widgets_2.8.0"       % "0.1-SNAPSHOT"
  val w20s    = "com.razie" % "20widgets-swing_2.8.0" % "0.1-SNAPSHOT"
  val razWeb  = "com.razie" % "razweb_2.8.0"          % "0.1-SNAPSHOT"
  val scrip   = "com.razie" % "scripster_2.8.1-SNAPSHOT"       % "0.4-SNAPSHOT"

  override def unmanagedClasspath = 
    (Path.fromFile ("../razbase/lib") / "json.jar") +++
      super.unmanagedClasspath 
 
  override def mainScalaSourcePath = "src"
  override def testScalaSourcePath = "test_src"
}

