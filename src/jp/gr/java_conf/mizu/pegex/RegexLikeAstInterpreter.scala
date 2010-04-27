package jp.gr.java_conf.mizu.pegex
import scala.collection.mutable.{Map => MutableMap, HashMap}

/**
  * This class represents interpreters by traversal of ASTs.
  * @author Kota Mizushima */
class RegexLikeAstInterpreter(grammar: Ast.Grammar) extends Parser {
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
    node: Ast.Exp, resultBindings: MutableMap[Symbol, (Int, Int)],
    onSucc: () => Boolean, onFail: () => Boolean
  ): Boolean = {
    def _eval(node: Ast.Exp, onSucc: () => Boolean, onFail: () => Boolean): Boolean = node match {
      case Ast.Str(_, str) =>
        val len = str.length
        if(isEnd(cursor + len - 1)){
          onFail()
        }else {
          var i = 0
          while(i < len && str(i) == input(cursor + i)) i += 1
          if(i < len) onFail()
          else {
            cursor += len
            onSucc()
          }
        }
      case Ast.CharSet(_, positive, set) =>
        if(isEnd || (positive != set(input(cursor)))) onFail()
        else {
          cursor += 1
          onSucc()
        }
      case Ast.Wildcard(_) =>
        if(isEnd) onFail()
        else {
          cursor += 1
          onSucc()
        }
      case Ast.Rep0(_, body) =>
        def onSuccRep(f: () => Boolean): Boolean = {
          val start = cursor
          val nf: () => Boolean = () => {
            cursor = start
            if(onSucc()) true else f()
          }
          _eval(body, () => { onSuccRep(nf) }, nf)
        }
        onSuccRep(onSucc)
      case Ast.Rep1(_, body) =>
        def onSuccRep(f: () => Boolean): Boolean = {
          val start = cursor
          val nf: () => Boolean = () => {
            cursor = start
            if(onSucc()) true else f()
          }
          _eval(body, () => { onSuccRep(nf) }, nf)
        }
        _eval(body, 
          () => { onSuccRep(onSucc) },
          onFail
        )
      case Ast.Opt(_, body) =>
        val start = cursor
        val onFailAlt: () => Boolean = () => {
          cursor = start; onSucc() 
        }
        _eval(body, 
          () => { if(onSucc()) true else onFailAlt() }, 
          onFailAlt
        )
      case Ast.AndPred(_, body) =>
        val start: Int = cursor
        _eval(body,
          () => { cursor = start; onSucc() },
          onFail
        )
      case Ast.NotPred(_, body) =>
        val start: Int = cursor
        _eval(body,
          onFail,
          () => { cursor = start; onSucc() }
        )
      case Ast.Seq(_, e1, e2) =>
        _eval(e1, () => { _eval(e2, onSucc, onFail) }, onFail)
      case Ast.Alt(_, e1, e2) =>
        val start = cursor
        val onFailAlt: () => Boolean = () => {
          cursor = start; _eval(e2, onSucc, onFail)
        }
        _eval(e1,
          () => { if(onSucc()) true else onFailAlt() },
          onFailAlt
        )
      case Ast.Ident(_, name) =>
        eval(ruleBindings(name), new HashMap, onSucc, onFail)
      case Ast.Binder(_, name, exp) =>
        val start = cursor
        _eval(exp,
          () => {
            resultBindings(name) = (start, cursor)
            onSucc()
          },
          onFail
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
          onSucc()
        }else {
          onFail()
        }
      case Ast.CharClass(_, _, _) => error("must not reach here")
    }
    _eval(node, onSucc, onFail)
  }
  def parseWithGroup(inputStr: String): MatchResult = {
    cursor =  0
    input = inputStr
    val map = new HashMap[Symbol, (Int, Int)]
    if(eval(ruleBindings(grammar.start), map, () => true, () => false)) {
      val result = Some(inputStr.substring(0, cursor))
      MatchResult(
        result, map.foldLeft(Map[Symbol, String]()){case (m, (k, v)) =>
          m + (k -> inputStr.substring(v._1, v._2))        
        }
      )
    }else MatchResult(None, Map.empty)
  }
}
