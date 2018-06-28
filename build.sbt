name := "asciiblog"

version := "0.1"

scalaVersion := "2.12.2"

assemblyJarName in assembly := "../../asciiblog.jar"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "com.maxmind.geoip" % "geoip-api" % "1.3.1",
  "org.spire-math" %% "jawn-ast" % "0.11.0",
  "net.sourceforge.cssparser" % "cssparser" % "0.9.25"
  //"com.medallia.word2vec" % "Word2VecJava" % "0.10.3"
)

fork := true
