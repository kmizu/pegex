package org.onion_lang.pegex
import java.io.File
import scala.io.Source

object LineCounter {
  def foreachFile(file: File, suffix: String)(block: File => Unit) {
    if(file.isDirectory) {
      file.listFiles().foreach(foreachFile(_, suffix)(block))
    }else {
      if(file.getName.endsWith(suffix)) block(file)
    }
  }
  def main(args: Array[String]) {
    var count = 0
    foreachFile(new File("src"), ".scala"){file =>
      val source = Source.fromFile(file)
      count += source.getLines.toList.size
      source.close()
    }
    println(count)
  }
}
