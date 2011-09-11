import sbt._

class PGremlins(info: ProjectInfo) extends DefaultProject(info) with posterous.Publish {
  override def managedStyle = ManagedStyle.Maven
  val publishTo =
    if (version.toString endsWith "-SNAPSHOT")
      "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/snapshots/"
    else
      "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/releases/"
  Credentials(Path.userHome / ".ivy2.credentials", log)
     
  val snap = (if (version.toString endsWith "-SNAPSHOT") "-SNAPSHOT" else "")
 
  val scalatest = "org.scalatest" % "scalatest_2.9.1" % "1.6.1"
  val junit =     "junit"         % "junit"     % "4.5" % "test->default"
  def json =      "org.json"      % "json"      % "20090211"

  val scripster   = "com.razie" %% "scripster"       % ("0.8" + snap)
}

