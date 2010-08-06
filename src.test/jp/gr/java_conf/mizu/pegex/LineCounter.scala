package jp.gr.java_conf.mizu.pegex
import java.io.File
import scala.io.Source

object LineCounter {
  def eachFile(file: File, suffix: String)(block: File => Unit) {
    if(file.isDirectory) {
      file.listFiles().foreach(eachFile(_, suffix)(block))
    }else {
      if(file.getName.endsWith(suffix)) block(file)
    }
  }
  def main(args: Array[String]) {
    var count = 0
    eachFile(new File("src"), ".scala"){file =>
      val source = Source.fromFile(file)
      count += source.getLines.toList.size
      source.close()
    }
    println(count)
  }
}
