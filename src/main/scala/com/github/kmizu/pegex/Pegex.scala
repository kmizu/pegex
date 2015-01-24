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
  class RichString(pattern: String) {
    def e: Pegex = new Pegex(pattern)
  }
  implicit def toPeg(pattern: String): RichString = new RichString(pattern)
  def apply(pattern: String): Pegex = new Pegex(pattern)
}