package jp.gr.java_conf.mizu.pegex

class Pegex(pattern: String, likeRegex: Boolean = true) {
  private[this] val interpreter = if(!likeRegex){
    new PegVm(
      PegToInsnCompiler.compile(
        PegexParser.parse(pattern)
      )
    )
  }else {
    new RegexLikeAstInterpreter(
      PegexParser.parse(pattern)
    )
  }
  def matches(input: String): Option[String] = {
    interpreter.parse(input)
  }
  def matchesWithGroup(input: String): MatchResult = {
    interpreter.parseWithGroup(input)
  }
}
object Pegex {
  class RichString(pattern: String) {
    def e: Pegex = new Pegex(pattern)
    def e(likeRegex: Boolean = true): Pegex = new Pegex(pattern, likeRegex)
  }  
  implicit def toPeg(pattern: String): RichString = new RichString(pattern)
  def apply(pattern: String): Pegex = new Pegex(pattern)
}