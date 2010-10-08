import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) {

  override def managedStyle = ManagedStyle.Maven
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  Credentials(Path.userHome / ".ivy2.credentials", log)
      
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
  val junit =     "junit"         % "junit"     % "4.5" % "test->default"
  def json =      "org.json"      % "json"      % "20090211"

  val scrip   = "com.razie" %% "scripster"       % "0.4-SNAPSHOT"
}

