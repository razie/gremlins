import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) with posterous.Publish {

  override def managedStyle = ManagedStyle.Maven
  //val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
  val publishTo = "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2.credentials", log)
      
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.6.1"
  val junit =     "junit"         % "junit"     % "4.5" % "test->default"
  def json =      "org.json"      % "json"      % "20090211"

  val scripster   = "com.razie" %% "scripster"       % "0.6"
  //val scripster   = "com.razie" %% "scripster"       % "0.6-SNAPSHOT"
}

