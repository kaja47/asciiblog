package asciiblog

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{ FileSystems, Paths, Path, WatchEvent }
import scala.jdk.CollectionConverters._


object Make extends App {
  util.requireConfig(args)

  try {
    val timer = new Timer().start()

    val (blog, base, resolver, changed) = MakeFiles.init(args)
    MakeFiles.makeFiles(blog, base, resolver, changed)

    println("total: "+timer.end().ms)

    blog.hooks.afterGenerate(base, blog)

  } catch {
    case e: ConfigurationException =>
      println("configuration error: "+e.message)
  }
}


object Watch extends App {{
  util.requireConfig(args)

  def generate(): Blog = {
    val timer = new Timer().start()
    val (blog, base, resolver, changed) = MakeFiles.init(args, _.quiet)
    MakeFiles.makeFiles(blog, base, resolver, changed)
    println("TOTAL TIME: "+timer.end().ms+"\n")
    blog
  }

  def isChanged(events: Seq[WatchEvent[_]]) =
    events.map { e => e.context.asInstanceOf[Path].toString }
      .exists { f => !f.startsWith(".") && !f.endsWith("~") }

  var blog    = generate()
  val watcher = FileSystems.getDefault.newWatchService

  // register cfg file
  Paths.get(args.head).getParent.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)

  // register directories
  blog.files.map(_.getParentFile).distinct.foreach { d => d.toPath.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY) }

  while (true) {
    val key = watcher.take()
    Thread.sleep(20) // wait a bit for editors to finish writing all files

    var changed = isChanged(key.pollEvents().asScala.toSeq)
    key.reset()

    // drain rest of related events
    var k = watcher.poll()
    while (k != null) {
      changed |= isChanged(k.pollEvents().asScala.toSeq)
      k.reset()
      k = watcher.poll()
    }

    if (changed) {
      blog = generate()
    }
  }
}}
