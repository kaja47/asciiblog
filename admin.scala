package asciiblog

import java.io. { File, FileWriter }
import jawn.ast._

object Admin extends App {

  val commentsDir = MakeFiles.keyValues(args(0))("remoteDir")+".comments"

  def mangle(f: String) = f.replaceAll("/", "_")
  def jsons(f: File): Seq[JValue] = io.Source.fromFile(f).getLines.toVector.map(l => JParser.parseFromString(l).get) 

  val globalFile = new File(commentsDir, "global.rss.html")
  val ipBlacklistFile = new File(commentsDir, "ip-blacklist")
  val deletedCommentsFile = new File(commentsDir, "deleted-comments")

  def readGlobal() = jsons(globalFile)

  def write(f: File, comments: Seq[JValue]) = {
    val fw = new FileWriter(f)
    for (c <- comments) fw.write(c.render+"\n")
    fw.close()
  }

  def confirm(message: String = "") = {
    if (message.nonEmpty) { println(message) }
    println("confirm [y/n]")
    io.StdIn.readLine().startsWith("y")
  }

  def printComments(cs: Seq[JValue]) {
    cs.zipWithIndex.map { case (j, i) =>
      "#"+i+" "+j.get("name").asString+" --- "+
      j.get("text").asString+"\n"
    }.foreach(println)
  }

  var global: Seq[JValue] = readGlobal()
  def globalComments = global.tail
  printComments(globalComments)

  val deleteAndBlockRegex = """d(!)? (\d+)""".r

  while (true) {
    io.StdIn.readLine() match {
      case "q" => sys.exit()
      case deleteAndBlockRegex(ex, i) =>
        val idx = i.toInt
        val block = ex == "!"

        val id   = globalComments(idx).get("id").asInt
        val ip   = globalComments(idx).get("ip").asString
        val text = globalComments(idx).get("text").asString
        val path = globalComments(idx).get("path").asString

        val msg = "delete "+text+(if (block) " and block ip "+ip else "")

        if (confirm(msg)) {
          val f = new File(commentsDir, mangle(path))
          val js = jsons(f)

          val delIdx = js.indexWhere(_.get("id").asInt == id, 1)

          val fw = new FileWriter(deletedCommentsFile, true)
          fw.write(js(delIdx).render+"\n")
          fw.close()

          val deleted = js.patch(delIdx, Seq(), 1)
          write(f, deleted)

          {
            val delIdx = global.indexWhere(_.get("id").asInt == id, 1)
            val deleted = global.patch(delIdx, Seq(), 1)
            write(globalFile, deleted)
          }

          if (block) {
            val fw = new FileWriter(ipBlacklistFile, true)
            fw.write(ip+"\n")
            fw.close()
          }
        }

        global = readGlobal()
        printComments(globalComments)

      case "u" =>
        global = readGlobal()
        printComments(globalComments)

      case _ =>
        println("unknown command (q, u, d $number, d! $number)")
    }
  }
}
