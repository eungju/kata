name := "CodeDojo"
scalaVersion := "2.11.4"
scalacOptions ++= Seq("-deprecation", "-feature")
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0"
  , "org.specs2" %% "specs2-core" % "2.4.15" % "test"
)
sourcesInBase := false
scalaSource in Test := baseDirectory.value
