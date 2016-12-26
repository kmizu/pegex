package com
package github
package kmizu
package pegex

import scala.collection.mutable.{Map => MutableMap, HashMap}

/**
  * This class represents interpreters by traversal of ASTs.
  * @author Kota Mizushima
  */
class PegexEvaluator(grammar: AstNode.Grammar) {
  private[this] val ruleBindings = Map(grammar.rules.map{r => (r.name, expand(r.body))}:_*)
  private[this] var cursor = 0
  private[this] var input: String = null
  private def isEnd: Boolean = cursor == input.length
  private def isEnd(pos: Int): Boolean = pos >= input.length
  private def expand(node: AstNode.Expression): AstNode.Expression = node match {
    case AstNode.CharacterClass(pos, positive, elems) =>
      AstNode.CharacterSet(pos, positive, elems.foldLeft(Set[Char]()){
        case (set, AstNode.CharacterRange(f, t)) => (set /: (f to t))((set, c) => set + c)
        case (set, AstNode.OneCharacter(c)) => set + c
      })
    case AstNode.Choice(pos, e1, e2) => AstNode.Choice(pos, expand(e1), expand(e2))
    case AstNode.Sequence(pos, e1, e2) => AstNode.Sequence(pos, expand(e1), expand(e2))
    case AstNode.Repeat0(pos, body) => AstNode.Repeat0(pos, expand(body))
    case AstNode.Repeat1(pos, body) => AstNode.Repeat1(pos, expand(body))
    case AstNode.Optional(pos, body) => AstNode.Optional(pos, expand(body))
    case AstNode.AndPredicate(pos, body) => AstNode.AndPredicate(pos, expand(body))
    case AstNode.NotPredicate(pos, body) => AstNode.NotPredicate(pos, expand(body))
    case AstNode.Binder(pos, n, e) => AstNode.Binder(pos, n, expand(e))
    case e => e
  }
  private def eval(
                    node: AstNode.Expression, resultBindings: MutableMap[Symbol, (Int, Int)], onSuccess: () => Boolean
  ): Boolean = {
    //TODO Consider using trampoline style, instead of direct CPS
    def _eval(node: AstNode.Expression)(onSuccess: => Boolean): Boolean = node match {
      case AstNode.StringLiteral(_, str) =>
        val len = str.length
        if(isEnd(cursor + len - 1)){
          false
        }else {
          var i = 0
          while(i < len && str(i) == input(cursor + i)) i += 1
          if(i < len) {
            false
          } else {
            cursor += len
            onSuccess
          }
        }
      case AstNode.CharacterSet(_, positive, set) =>
        if(isEnd || (positive != set(input(cursor)))) {
          false
        } else {
          cursor += 1
          onSuccess
        }
      case AstNode.Wildcard(_) =>
        if(isEnd) {
          false
        } else {
          cursor += 1
          onSuccess
        }
      case AstNode.Start(_) =>
        if(cursor == 0) {
          onSuccess
        } else {
          false
        }
      case AstNode.Repeat0(_, body) =>
        def onSuccRep(f: => Boolean): Boolean = {
          val start = cursor
          def nf: Boolean = {
            cursor = start
            if(onSuccess) true else f
          }
          if(_eval(body){
            val previous = start
            val current = cursor
            if(previous == current) {
              nf
            } else {
              onSuccRep(nf)
            }
          }) {
            true
          } else {
            nf
          }
        }
        onSuccRep(onSuccess)
      case AstNode.Repeat1(_, body) =>
        def onSuccRep(f: => Boolean): Boolean = {
          val start = cursor
          def nf: Boolean = {
            cursor = start
            if(onSuccess) true else f
          }
          if(_eval(body){
            val previous = start
            val current = cursor
            if(previous == current) {
              nf
            } else {
              onSuccRep(nf)
            }
          }) {
            true
          } else {
            nf
          }
        }
        _eval(body){ onSuccRep(onSuccess) }
      case AstNode.Optional(_, body) =>
        val start = cursor
        if(_eval(body)(onSuccess)) {
          true
        } else {
          cursor = start
          onSuccess
        }
      case AstNode.AndPredicate(_, body) =>
        val start = cursor
        _eval(body){ cursor = start; onSuccess }
      case AstNode.NotPredicate(_, body) =>
        val start = cursor
        if(_eval(body){ true }) {
          false
        } else {
          cursor = start
          onSuccess
        }
      case AstNode.Sequence(_, e1, e2) =>
        _eval(e1){ _eval(e2)(onSuccess) }
      case AstNode.Choice(_, e1, e2) =>
        val start = cursor
        if(_eval(e1)(onSuccess)) {
          true
        } else {
          cursor = start
          _eval(e2)(onSuccess)
        }
      case AstNode.Identifier(_, name) =>
        eval(ruleBindings(name), new HashMap, () => onSuccess)
      case AstNode.Binder(_, name, exp) =>
        val start = cursor
        _eval(exp) {
          resultBindings(name) = (start, cursor)
          onSuccess
        }
      case AstNode.Backreference(_, name) =>
        val (start, end) = resultBindings(name)
        def matches(): Boolean = {
          (0 until (end - start)).forall{i =>
            (!isEnd(cursor + i)) && input(start + i) == input(cursor + i)
          }
        }
        val successful = matches()
        if(successful) {
          cursor += (end - start)
          onSuccess
        }else {
          false
        }
      case AstNode.CharacterClass(_, _, _) => sys.error("must not reach here")
    }
    _eval(node)(onSuccess())
  }

  /**
   * Parses input and returns a parse result.
   * @param inputStr the input string
   * @return the parse result, which is MatchResult.
   */
  def parse(inputStr: String): MatchResult = this.synchronized {
    cursor =  0
    input = inputStr
    val map = new HashMap[Symbol, (Int, Int)]
    if(eval(ruleBindings(grammar.start), map, () => true)) {
      val result = Some(inputStr.substring(0, cursor))
      MatchResult(
        result, map.foldLeft(Map[Symbol, String]()){case (m, (k, v)) =>
          m + (k -> inputStr.substring(v._1, v._2))
        }
      )
    }else MatchResult(None, Map.empty)
  }

  /**
   * Parses input and returns a parse result.
   * @param input the input string
   * @return the parse result, which is Some[String] or None.
   */
  def matches(input: String): Option[String] = {
    parse(input).result
  }
}
