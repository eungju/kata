name := "HackerRank"

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.12" % "test",
  "org.hamcrest" % "hamcrest-library" % "1.3" % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test->default"
)
