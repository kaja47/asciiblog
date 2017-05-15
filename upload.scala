import java.io.File
import java.nio.file.StandardCopyOption._
import java.nio.file.Files

if (args.length != 2) {
  println("usage scala upload.scala source-directory destination-directory")
  sys.exit()
}

val source = new File(args(0))
val target = new File(args(1))

def parseFileIndex(lines: Iterator[String]): Map[String, String] =
  lines.map { l => val Array(f, h) = l.split(" ", 2) ; (f, h) }.toMap

def readFileIndex(f: File): Map[String, String] =
  if (f.exists) {
    parseFileIndex(io.Source.fromFile(f).getLines)
  } else {
    Map()
  }

val sourceIndex = readFileIndex(new File(source, ".files"))
val targetIndex = readFileIndex(new File(target, ".files"))

for {
  s <- source.listFiles.sortBy(f => (f.getName == ".files", f.getName)) // file index last
  fn = s.getName
  if fn.matches("""(?x) ( .*\.html | .*\.xml | .*\.js | robots.txt | \.files )""")
  if sourceIndex.contains(fn)
} {
  val t = new File(target, fn)

  if (!targetIndex.contains(fn) || targetIndex(fn) != sourceIndex(fn)) {
    println(s"copying $s -> $t")
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  } else {
    //println(s" skipping $s -> $t")
  }
}

val thumbSource = new File(source, "t")
val thumbTarget = new File(target, "t")
thumbTarget.mkdirs()

for (s <- thumbSource.listFiles) {
  val t = new File(thumbTarget, s.getName)
  if (!t.exists) {
    println(s"copying $s -> $t")
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  } else {
    //println(s" skipping $s -> $t")
  }

}
