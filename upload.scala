package asciiblog

import java.io.File
import java.nio.file.StandardCopyOption._
import java.nio.file.{ Files, NoSuchFileException }

object Upload extends App { timer("upload") {

val cfgFile = new File(args(0))
val cfg = MakeFiles.readConfig(cfgFile)
val blog = Blog.populate(cfg, args, Map(), cfgFile)

val source = blog.outDir
val target = new File(cfg("remoteDir"))


// copy images

val thumbSource = new File(source, "t")
val thumbTarget = new File(target, "t")

val cardsSource = new File(thumbSource, "card")
val cardsTarget = new File(thumbTarget, "card")

def copyDirectory(source: File, target: File): Unit = {
  if (!source.exists()) return

  target.mkdirs()
  for (s <- source.listFiles) {
    val t = new File(target, s.getName)
    if (!t.exists && !s.isDirectory) {
      println(s"copying $s -> $t")
      Files.copy(s.toPath, t.toPath, REPLACE_EXISTING)
    }
  }
}

copyDirectory(thumbSource, thumbTarget)
copyDirectory(cardsSource, cardsTarget)


// copy html files

def parseFileIndex(lines: Iterator[String]): Map[String, String] =
  lines.map { l => val Array(h, f) = l.split(" ", 2) ; (f, h) }.toMap

// [file -> hash]
def readFileIndex(f: File, retries: Int = 3): Map[String, String] =
  if (f.exists) {
    parseFileIndex(io.Source.fromFile(f).getLines)
  } else if (retries > 0) {
    // sometimes files from mounted remote directory are reported as missing during first access
    readFileIndex(f, retries-1)
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

move(sf, tf, 5)

} }
