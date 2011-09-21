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

  val SCALAVER = "2.9.1"

  def scalatest = "org.scalatest" % "scalatest_2.9.1" % "1.6.1"
  def junit =     "junit"         % "junit"     % "4.5" % "test->default"
  def json =      "org.json"      % "json"      % "20090211"

  def scripster   = "com.razie" %% "scripster"       % ("0.8" + snap)

  lazy val core = project("core", "gremlins-core", new CoreProject(_))
  //lazy val gakka  = project("akka",  "gremlins-akka",  new GakkaProject(_), core)

  class CoreProject(info: ProjectInfo) extends DefaultProject(info) {
    val depys@(sc, ju, js, scr) = (scalatest, junit, json, scripster)
  }

}

