package org.onion_lang.pegex
import java.io._
import scala.collection.immutable._
import Pegex._

/**
  * This object provides a tool for pattern matching
  * using Pegex.
  * @author Kota Mizushima */
object PegexMatcher {
  def readAll(reader: Reader): String = {
    val buf = new StringBuilder
    var ch = 0
    while({ch = reader.read; ch} != -1) {
      buf.append(ch.asInstanceOf[Char])
    }
    buf.toString
  }
  def open[A](file :String)(f : Reader => A): A = {
    val r = new FileReader(file)
    try {
      f(r)
    } finally {
      r.close
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
          val pattern = readLine("pattern> ")
          if(pattern == "exit" || pattern == "") return
          try {
            val pegex = pattern.e
            val input = readLine("input> ")
            println(pegex.matches(input).getOrElse("not matched"))
          }catch{
            case e:Exception => println(e.getMessage)
          }
        }
      case List("-m", pattern, input) => 
        println(pattern.e.matches(input))
      case opt :: grammarFile :: inputs if inputs.length >= 1 =>
        inputs.foreach{input =>
          val grammar = PegParser.parse(grammarFile, new FileReader(grammarFile))
          val insns = PegToInsnCompiler.compile(grammar)
          val interpreter: Parser =
            if(opt == "-vm")
              new GreedyPegVirtualMachine(insns)
            else if(opt == "-ast")
              new GreedyPegInterpreter(grammar)
            else error("not implemented")
          open(input){reader =>
            val inputStr = readAll(reader)
            System.err.println("parsing " + input)     
            logTime{
              interpreter.matches(inputStr)
            }
          }
        }
      case _ =>
        println("""Usage: java -jar pegex.jar (-vm|-ast) <file_name> <input>
                  |   or  java -jar pegex.jar -m <pegex_pattern> <input_string>""".stripMargin)
    }
  }
}
