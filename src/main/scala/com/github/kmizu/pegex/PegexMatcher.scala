package com
package github
package kmizu
package pegex

import java.io._
import scala.collection.immutable._
import scala.util.control.Breaks._
import Pegex._

/**
  * This object provides a tool for pattern matching
  * using Pegex.
  * @author Kota Mizushima
  */
object PegexMatcher {
  def readAll(reader: Reader): String = {
    val buf = new StringBuilder
    var ch = 0
    while({ch = reader.read; ch} != -1) {
      buf.append(ch.asInstanceOf[Char])
    }
    buf.toString()
  }
  def open[A](file :String)(f : Reader => A): A = {
    val r = new FileReader(file)
    try {
      f(r)
    } finally {
      r.close()
    }
  }
  def logTime[A](a: => A): A = {
    val start = System.currentTimeMillis()
    val result = a
    System.err.println("time " + (System.currentTimeMillis() - start) + "ms")
    result
  }
  def main(args: Array[String]){
    args.toList match {
      case Nil =>
        //REPL mode
        while(true) {
          val pattern = io.StdIn.readLine("pattern> ")
          if(pattern == ":quit" || pattern == "") return
          try {
            val pegex = pattern.e
            breakable {
              while(true) {
                val input = io.StdIn.readLine("input> ")
                if(input == ":quit") break()
                println(pegex.matches(input).map("matched: " + _).getOrElse("not matched"))
              }
            }
          }catch{
            case e:Exception => println(e.getMessage)
          }
        }
      case List("-m", pattern, input) => 
        println(pattern.e.matches(input))
      case grammarFile :: inputs if inputs.length >= 1 =>
        inputs.foreach{input =>
          val grammar = PegexParser.parse(grammarFile, new FileReader(grammarFile))
          val interpreter: Recognizer = new PegexEvaluator(grammar)
          open(input){reader =>
            val inputStr = readAll(reader)
            System.err.println("parsing " + input)     
            logTime{
              interpreter.matches(inputStr)
            }
          }
        }
      case _ =>
        println("""Usage: java -jar pegex.jar <file_name> <input>
                  |   or  java -jar pegex.jar -m <pegex_pattern> <input_string>""".stripMargin)
    }
  }
}
