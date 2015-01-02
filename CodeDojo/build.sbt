name := "CodeDojo"
scalaVersion := "2.11.4"
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0"
  , "org.specs2" %% "specs2-core" % "2.4.15" % "test"
  //, "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)
sourcesInBase := false
scalaSource in Test := baseDirectory.value
