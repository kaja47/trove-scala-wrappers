import sbt._
import Keys._


object TheBuild extends Build {

  override lazy val settings = super.settings ++
    Seq(
      name := "trove-scala-wrappers",
      organization := "atrox",
      version := "0.1",

      scalaVersion := "2.11.6",
      //scalaVersion := "2.11.0-M7",
      resolvers ++= Seq(
        "Typesafe Repository"    at "http://repo.typesafe.com/typesafe/releases/",
        "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
        "Sonatype Releases"      at "https://oss.sonatype.org/content/repositories/releases/"
      ),
      libraryDependencies ++= Seq(
        "net.sf.trove4j"             % "trove4j"              % "3.0.3"
      ),
      sourceGenerators in Compile := Seq(),
      sourceManaged in Compile := baseDirectory.value / "gen",
      sourceGenerators in Compile <+= Def.task {
        val file = (sourceManaged in Compile).value / "trove-genrated.scala"
        IO.write(file, Gen.genTrove)
        Seq(file)
      }
    )

  lazy val root = Project(
    id       = "trove-scala-wrappers",
    base     = file("."),
    settings = Project.defaultSettings ++ TheBuild.settings
  )
}
