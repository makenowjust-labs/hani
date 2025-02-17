Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:all",
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "hani",
    scalaVersion := "3.6.2",
    console / initialCommands :=
      """|import scala.util.Random
         |
         |import hani.*
         |""".stripMargin,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test,
    doctestTestFramework := DoctestTestFramework.Munit
  )
