package jp.gr.java_conf.mizu.pegex
import scala.collection.mutable.{Map => MutableMap, HashMap}

/**
  * This class represents interpreters by traversal of ASTs.
  * @author Kota Mizushima */
class PegAstInterpreter(grammar: Ast.Grammar) extends Parser {
  private[this] val bindings = Map(grammar.rules.map{r => (r.name, expand(r.body))}:_*)
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
    case e => e
  }
  private def eval(node: Ast.Exp, bindingsForBackref: MutableMap[Symbol, (Int, Int)]): Boolean = {
    def _eval(node: Ast.Exp): Boolean = node match {
      case Ast.Str(_, str) =>
        val len = str.length
        if(isEnd(cursor + len - 1)){
          return false
        }else {
          var i = 0
          while(i < len && str(i) == input(cursor + i)) i += 1
          if(i < len) false
          else {
            cursor += len
            true
          }
        }
      case Ast.CharSet(_, positive, set) =>
        if(isEnd || (positive != set(input(cursor)))) false
        else {
          cursor += 1
          true
        }
      case Ast.Wildcard(_) =>
        if(isEnd) false
        else {
          cursor += 1
          true
        }
      case Ast.Rep0(_, body) =>
        var start: Int = cursor
        while(_eval(body)) {
          start = cursor
        }
        cursor = start
        true
      case Ast.Rep1(_, body) =>
        if(!_eval(body)) false
        else {
          var start: Int = cursor
          while(_eval(body)) start = cursor
          cursor = start
          true
        }
      case Ast.Opt(_, body) =>
        var start: Int = cursor
        if(!_eval(body)) {
          cursor = start
        }
        true
      case Ast.AndPred(_, body) =>
        val start: Int = cursor
        if(_eval(body)) {
          cursor = start
          true
        } else false
      case Ast.NotPred(_, body) =>
        val start: Int = cursor
        if(!_eval(body)) {
          cursor = start
          true
        } else false
      case Ast.Seq(_, e1, e2) =>
        _eval(e1) && _eval(e2)
      case Ast.Alt(_, e1, e2) =>
        val start = cursor
        if(_eval(e1)) {
          true
        }else {
          cursor = start
          _eval(e2)
        }
      case Ast.Ident(_, name, backref) =>
        backref match {
          case Some(ref) =>
            val start = cursor       
            val successful = eval(bindings(name), new HashMap)
            if(successful) {
              bindingsForBackref(ref) = (start, cursor)
            }
            successful
          case None =>
            eval(bindings(name), new HashMap)
        }
      case Ast.Binder(_, name, exp) =>
        val start = cursor
        val successful = _eval(exp)
        if(successful) {
          bindingsForBackref(name) = (start, cursor)
        }
        successful
      case Ast.Backref(_, name) =>
        val (start, end) = bindingsForBackref(name)
        def matches(): Boolean = {
          (0 until (end - start)).forall{i =>
            (!isEnd(cursor + i)) && input(start + i) == input(cursor + i)
          }
        }
        val successful = matches()
        if(successful) {
          cursor += (end - start)
        }
        successful
      case Ast.CharClass(_, _, _) => error("must not reach here")
    }
    _eval(node)
  }
  def parseWithGroup(inputStr: String): MatchResult = {
    cursor =  0
    input = inputStr
    val map = new HashMap[Symbol, (Int, Int)]
    if(eval(bindings(grammar.start), map)) {
      val result = Some(inputStr.substring(0, cursor))
      MatchResult(
        result, map.foldLeft(Map[Symbol, String]()){case (m, (k, v)) =>
          m + (k -> inputStr.substring(v._1, v._2))        
        }
      )
    }else {
      MatchResult(None, Map.empty)
    }
  }
}
