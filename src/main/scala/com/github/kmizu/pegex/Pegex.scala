package com
package github
package kmizu
package pegex

class Pegex(pattern: String) {
  private[this] val interpreter = {
    new PegexEvaluator(
      PegexParser.parse(pattern)
    )
  }
  def matches(input: String): Option[String] = {
    interpreter.matches(input)
  }
  def matchesWithGroup(input: String): MatchResult = {
    interpreter.parse(input)
  }
}
object Pegex {
  implicit class RichString(val pattern: String) extends AnyVal {
    def e: Pegex = new Pegex(pattern)
  }

  def apply(pattern: String): Pegex = new Pegex(pattern)
}