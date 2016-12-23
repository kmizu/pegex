package com
package github
package kmizu
package pegex

import scala.collection.mutable.{Map => MutableMap, HashMap}

/**
  * This class represents interpreters by traversal of ASTs.
  * @author Kota Mizushima
  */
class PegexEvaluator(grammar: Ast.Grammar) {
  private[this] val ruleBindings = Map(grammar.rules.map{r => (r.name, expand(r.body))}:_*)
  private[this] var cursor = 0
  private[this] var input: String = null
  private def isEnd: Boolean = cursor == input.length
  private def isEnd(pos: Int): Boolean = pos >= input.length
  private def expand(node: Ast.Exp): Ast.Exp = node match {
    case Ast.CharClass(pos, positive, elems) => 
      Ast.CharSet(pos, positive, elems.foldLeft(Set[Char]()){       
        case (set, Ast.CharRange(f, t)) => (set /: (f to t))((set, c) => set + c)
        case (set, Ast.OneChar(c)) => set + c
      })
    case Ast.Alt(pos, e1, e2) => Ast.Alt(pos, expand(e1), expand(e2))
    case Ast.Seq(pos, e1, e2) => Ast.Seq(pos, expand(e1), expand(e2))
    case Ast.Rep0(pos, body) => Ast.Rep0(pos, expand(body))
    case Ast.Rep1(pos, body) => Ast.Rep1(pos, expand(body))
    case Ast.Opt(pos, body) => Ast.Opt(pos, expand(body))
    case Ast.AndPred(pos, body) => Ast.AndPred(pos, expand(body))
    case Ast.NotPred(pos, body) => Ast.NotPred(pos, expand(body))
    case Ast.Binder(pos, n, e) => Ast.Binder(pos, n, expand(e))
    case e => e
  }
  private def eval(
    node: Ast.Exp, resultBindings: MutableMap[Symbol, (Int, Int)], onSuccess: () => Boolean
  ): Boolean = {
    //TODO Consider using trampoline style, instead of direct CPS
    def _eval(node: Ast.Exp, onSuccess: () => Boolean): Boolean = node match {
      case Ast.Str(_, str) =>
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
            onSuccess()
          }
        }
      case Ast.CharSet(_, positive, set) =>
        if(isEnd || (positive != set(input(cursor)))) {
          false
        } else {
          cursor += 1
          onSuccess()
        }
      case Ast.Wildcard(_) =>
        if(isEnd) {
          false
        } else {
          cursor += 1
          onSuccess()
        }
      case Ast.Rep0(_, body) =>
        def onSuccRep(f: () => Boolean): Boolean = {
          val start = cursor
          val nf: () => Boolean = () => {
            cursor = start
            if(onSuccess()) true else f()
          }
          if(_eval(body, () => { onSuccRep(nf) })) {
            true
          } else {
            nf()
          }
        }
        onSuccRep(onSuccess)
      case Ast.Rep1(_, body) =>
        def onSuccRep(f: () => Boolean): Boolean = {
          val start = cursor
          val nf: () => Boolean = () => {
            cursor = start
            if(onSuccess()) true else f()
          }
          if(_eval(body, () => { onSuccRep(nf) })) {
            true
          } else {
            nf()
          }
        }
        _eval(body, () => { onSuccRep(onSuccess) })
      case Ast.Opt(_, body) =>
        val start = cursor
        if(_eval(body, onSuccess)) {
          true
        } else {
          cursor = start
          onSuccess()
        }
      case Ast.AndPred(_, body) =>
        val start = cursor
        _eval(body, () => { cursor = start; onSuccess() })
      case Ast.NotPred(_, body) =>
        val start = cursor
        if(_eval(body, () => { true })) {
          false
        } else {
          cursor = start
          onSuccess()
        }
      case Ast.Seq(_, e1, e2) =>
        _eval(e1, () => { _eval(e2, onSuccess) })
      case Ast.Alt(_, e1, e2) =>
        val start = cursor
        if(_eval(e1, onSuccess)) {
          true
        } else {
          cursor = start
          _eval(e2, onSuccess)
        }
      case Ast.Ident(_, name) =>
        eval(ruleBindings(name), new HashMap, onSuccess)
      case Ast.Binder(_, name, exp) =>
        val start = cursor
        _eval(exp,
          () => {
            resultBindings(name) = (start, cursor)
            onSuccess()
          }
        )
      case Ast.Backref(_, name) =>
        val (start, end) = resultBindings(name)
        def matches(): Boolean = {
          (0 until (end - start)).forall{i =>
            (!isEnd(cursor + i)) && input(start + i) == input(cursor + i)
          }
        }
        val successful = matches()
        if(successful) {
          cursor += (end - start)
          onSuccess()
        }else {
          false
        }
      case Ast.CharClass(_, _, _) => sys.error("must not reach here")
    }
    _eval(node, onSuccess)
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
