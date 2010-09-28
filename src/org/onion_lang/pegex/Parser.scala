package org.onion_lang.pegex

/**
 * This trait represents parsers, which recognize inputs and return results.
 * @author Kota Mizushima */
trait Parser {
  /**
    * Parses inputStr and returns a parse result.
    * @param inputStr the input string  
    * @return the parse result, which is Some[String] or None.
    */
  def matches(inputStr: String): Option[String] = {
    parse(inputStr).result
  }
  
  def parse(inputStr: String): MatchResult
}
