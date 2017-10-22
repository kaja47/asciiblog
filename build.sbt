name := "asciiblog"

version := "0.1"

scalaVersion := "2.12.2"

assemblyJarName in assembly := "../../asciiblog.jar"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)
