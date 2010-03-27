package jp.gr.java_conf.mizu.pegex

/**
 * This trait represents parsers, which recognize inputs and return results.
 * @author Kota Mizushima */
trait Parser {
  /**
    * Parses inputStr and returns a parse result.
    * @param inputStr the input string  
    * @return the parse result, which is Some[String] or None.
    */
  def parse(inputStr: String): Option[String] = {
    parseWithGroup(inputStr).result
  }
  
  def parseWithGroup(inputStr: String): MatchResult
}
