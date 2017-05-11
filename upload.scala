import java.io.File
import java.nio.file.StandardCopyOption._
import java.nio.file.Files

if (args.length != 2) {
  println("usage scala upload.scala source-directory destination-directory")
  sys.exit()
}

val sourceDir = args(0)
val targetDir = args(1)

val source = new File(sourceDir)
val target = new File(targetDir)

def parseFileIndex(lines: Iterator[String]): Map[String, String] =
  lines.map { l => val Array(f, h) = l.split(" ", 2) ; (f, h) }.toMap

def readFileIndex(f: File): Map[String, String] =
  if (f.exists) {
    parseFileIndex(io.Source.fromFile(f).getLines)
  } else {
    Map()
  }

val sourceFileIndex = readFileIndex(new File(source, ".files"))
val targetFileIndex = readFileIndex(new File(target, ".files"))

for {
  s <- source.listFiles.sortBy(_.getName)
  if s.getName.matches("""(?x) ( .*\.html | .*\.xml | .*\.js | robots.txt | \.files )""")
} {
  val fn = s.getName
  val t = new File(target, fn)

  if (!targetFileIndex.contains(fn) || targetFileIndex(fn) != sourceFileIndex(fn)) {
    println(s"copying $s -> $t")
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  } else {
    println(s" skipping $s -> $t")
  }
}

val thumbSource = new File(sourceDir, "t")
val thumbTarget = new File(targetDir, "t")
thumbTarget.mkdirs()

for (s <- thumbSource.listFiles) {
  val t = new File(thumbTarget, s.getName)
  if (!t.exists) {
    println(s"copying $s -> $t")
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  } else {
    println(s" skipping $s -> $t")
  }

}
