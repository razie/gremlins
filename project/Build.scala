import sbt._
import Keys._

object V {
  val version      = "0.6.6-SNAPSHOT"
  val scalaVersion = "2.10.3"
  val organization = "com.razie"

  def snap = (if (V.version endsWith "-SNAPSHOT") "-SNAPSHOT" else "")
}

object MyBuild extends Build {

  def scalatest = "org.scalatest" %% "scalatest"        % "1.9.2"
  def junit     = "junit"          % "junit"            % "4.5"      % "test->default"
  def json      = "org.json"       % "json"             % "20090211"
  
  def akka      = "org.scala-lang" % "scala-actors"    % "2.10.3"
  
  def scripster = "com.razie"     %% "scripster"        % ("0.8.6" + V.snap)

  lazy val root = Project(id="gremlins",    base=file("."),
                          settings = defaultSettings ++ Seq()
                  ) aggregate (core) dependsOn (core)

  lazy val core = Project(id="gremlins-core", base=file("core"),
                          settings = defaultSettings ++ 
                          Seq(libraryDependencies ++= Seq(akka, scalatest, junit, json, scripster))
                  )

  def defaultSettings = baseSettings ++ Seq()

  def baseSettings = Defaults.defaultSettings ++ Seq (
    scalaVersion         := V.scalaVersion,
    version              := V.version,
    organization         := V.organization,
    organizationName     := "Razie's Pub",
    organizationHomepage := Some(url("http://www.razie.com")),

    publishTo <<= version { (v: String) =>
      if(v endsWith "-SNAPSHOT")
        Some ("Sonatype" at "https://oss.sonatype.org/content/repositories/snapshots/")
      else
        Some ("Sonatype" at "https://oss.sonatype.org/content/repositories/releases/")
    },

    resolvers ++= Seq("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                      "releases"  at "https://oss.sonatype.org/content/repositories/releases")    )


}
