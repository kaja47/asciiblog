package asciiblog

import java.io.File
import java.nio.file.StandardCopyOption._
import java.nio.file.{ Files, NoSuchFileException }

object Upload extends App {

if (args.length != 2) {
  println("usage scala upload.scala source-directory destination-directory")
  sys.exit()
}

val source = new File(args(0))
val target = new File(args(1))


// copy images

val thumbSource = new File(source, "t")
val thumbTarget = new File(target, "t")
thumbTarget.mkdirs()

for (s <- thumbSource.listFiles) {
  val t = new File(thumbTarget, s.getName)
  if (!t.exists) {
    println(s"copying $s -> $t")
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  }
}


// copy html files

def parseFileIndex(lines: Iterator[String]): Map[String, String] =
  lines.map { l => val Array(h, f) = l.split(" ", 2) ; (f, h) }.toMap

// [file -> hash]
def readFileIndex(f: File): Map[String, String] =
  if (f.exists) {
    parseFileIndex(io.Source.fromFile(f).getLines)
  } else {
    Map()
  }

val sf = new File(source, ".files")
val tf = new File(target, ".files")
val sourceIndex = readFileIndex(sf)
val targetIndex = readFileIndex(tf)

def move(s: File, t: File, retries: Int): Unit = {
  println(s"copying $s -> $t")
  try {
    if (t.getParentFile != null) {
      t.getParentFile.mkdirs()
    }
    Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
  } catch {
    case e: NoSuchFileException if retries > 0 => move(s, t, retries-1)
  }
}

for {
  (fn, _) <- sourceIndex.toSeq.sortBy { case (f, _) => if (f == "index.html" || f == "rss.xml") (1, f) else (0, f) }
} {
  val s = new File(source, fn)
  val t = new File(target, fn)
  if (!t.exists() || !targetIndex.contains(fn) || targetIndex(fn) != sourceIndex(fn)) {
    move(s, t, 5)
  }
}

Files.copy(sf.toPath, tf.toPath, REPLACE_EXISTING)

}
