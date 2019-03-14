name := "asciiblog"

version := "0.1"

scalaVersion := "2.12.8"

assemblyJarName in assembly := "../../asciiblog.jar"

assemblyMergeStrategy in assembly := {
  case PathList("scala", "concurrent", xs @ _*)               => MergeStrategy.discard
  case PathList("scala", "text", xs @ _*)                     => MergeStrategy.discard
  case PathList("scala", "sys", "process", xs @ _*)           => MergeStrategy.discard
  case PathList("scala", "ref", xs @ _*)                      => MergeStrategy.discard
  case PathList("scala", "beans", xs @ _*)                    => MergeStrategy.discard
//case PathList("scala", "collection", "script", xs @ _*)     => MergeStrategy.discard
  case PathList("scala", "collection", "parallel", xs @ _*)   => MergeStrategy.discard
//case PathList("scala", "collection", "concurrent", xs @ _*) => MergeStrategy.discard
  case x => ((assemblyMergeStrategy in assembly).value)(x)
}

sources in Compile ++= (baseDirectory.value / "examples" * "*.scala").get

libraryDependencies ++= Seq(
  "org.scala-lang.modules"    %% "scala-xml" % "1.0.6",
  "com.maxmind.geoip"         %  "geoip-api" % "1.3.1",
  "org.spire-math"            %% "jawn-ast"  % "0.13.0",
  "net.sourceforge.cssparser" %  "cssparser" % "0.9.26"
  //"com.medallia.word2vec" % "Word2VecJava" % "0.10.3"
)

fork := true
