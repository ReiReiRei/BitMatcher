name := "trade"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "1.0.1",
  "org.scalatest" %% "scalatest" % "3.0.3")
scalacOptions += "-Ypartial-unification"