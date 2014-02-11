package com
package github
package kmizu
package pegex

/**
 * This trait represents recognizer, which recognize inputs and return results.
 * @author Kota Mizushima
 */
trait Recognizer {
  /**
    * Parses input and returns a parse result.
    * @param input the input string
    * @return the parse result, which is Some[String] or None.
    */
  def matches(input: String): Option[String] = {
    parse(input).result
  }
  
  def parse(input: String): MatchResult
}
