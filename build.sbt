// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "0.1.0"

val chiselVersion = "3.5.0"

// For running the gradescope tests
libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.10.0" % "test"

// This sets it up so all tests that end in "Tester" will be run when you run sbt test
// and all tests that end in "Grader" will run when you run sbt Grader / test
lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.10"
lazy val TestAll = config("testAll") extend(Test)


def allFilter(name: String): Boolean = name endsWith "Tester"

lazy val root = (project in file("."))
.configs(TestAll)
  .settings(
    name := "bloomfilter",
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % "0.5.0" % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full),
    // from dino
    inConfig(TestAll)(Defaults.testTasks),

    libraryDependencies += scalatest % TestAll,

    testOptions in TestAll := Seq(Tests.Filter(allFilter)),
 )
