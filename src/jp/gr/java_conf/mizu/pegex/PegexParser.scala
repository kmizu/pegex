package jp.gr.java_conf.mizu.pegex
import scala.util.parsing.combinator._
import scala.util.parsing.input
import input._
import java.io._
import Ast._
/**
  * This object provides a parser that parses strings in Pegex and translates
  * them into ASTs of PEG.
  * @author Kota Mizushima */
object PegexParser {
  case class ParseException(pos: Pos, msg: String) extends Exception(pos.line + ", " + pos.column + ":" + msg)
  
  /*
   * See
   * Bryan Ford, "Parsing Expression Grammars: A Recognition-Based Syntactic Foundation",
   * Symposium on Principles of Programming Languages,2004.
   */
  object ParserCore extends Parsers {
    type Elem = Char 
    private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)
    private def chr(c: Char): Parser[Char] = c
    private def crange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)
    private def cset(cs: Char*): Parser[Char] = elem("[]", c => cs.findIndexOf(_ == c) >= 0)
    private val escape: Map[Char, Char] = Map(
      'n' -> '\n', 'r' -> '\r', 't' -> '\t', 'f' -> '\f'
    )
    private def not[T](p: => Parser[T], msg: String): Parser[Unit] = {
      not(p) | failure(msg)
    }
    lazy val GRAMMER: Parser[Grammar] = (loc <~ Spacing) ~ Definition.+ <~ EndOfFile ^^ {
      case pos ~ rules => Grammar(Pos(pos.line, pos.column), rules.head.name, rules)
    }
    lazy val Definition: Parser[Rule] = (Identifier <~ EQ) ~ 
      Expression <~ SEMI_COLON <~ Spacing ^^ {
      case n ~ b => Rule(n.pos, n.name, b)
    }  
    
    lazy val Expression: Parser[Exp] = rep1sep(Sequence, BAR) ^^ {ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => Alt(y.pos, a, y)}
    }
    lazy val Sequence: Parser[Exp] = Prefix.+ ^^ {ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => Seq(y.pos, a, y)}
    }
    lazy val Prefix: Parser[Exp] = (
      (loc <~ AND) ~ Suffix ^^ { case pos ~ e => AndPred(Pos(pos.line, pos.column), e) }
    | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e => NotPred(Pos(pos.line, pos.column), e) }
    | Suffix
    )
    lazy val Suffix: Parser[Exp] = (
      loc ~ Primary <~ QUESTION ^^ { case pos ~ e => Opt(Pos(pos.line, pos.column), e) }
    | loc ~ Primary <~ STAR ^^ { case pos ~ e => Rep0(Pos(pos.line, pos.column), e) }
    | loc ~ Primary <~ PLUS ^^ { case pos ~ e => Rep1(Pos(pos.line, pos.column), e) }
    | Primary
    )
    lazy val Primary: Parser[Exp] = (
      (chr('#') ~> chr('(')) ~> Identifier ~ 
      opt(chr(':') ~> chr(':') ~> Expression | chr(':') ~> Identifier) <~ chr(')') ^^ { 
        case ident ~ Some(exp) => Binder(ident.pos, ident.name, exp)
        case ident ~ None => ident
      }
    | OPEN ~> Expression <~ CLOSE
    | (chr('#') ~> chr('#') ~> chr('(')) ~> Identifier <~ chr(')') ^^ {ident => Backref(ident.pos, ident.name)}
    | CLASS
    | loc <~ DOLLAR ^^ { case pos => 
        val p = Pos(pos.line, pos.column); NotPred(p, Wildcard(p))
      }
    | loc <~ DOT ^^ { case pos => Wildcard(Pos(pos.line, pos.column)) }
    | loc <~ chr('_') ^^ { case pos => Str(Pos(pos.line, pos.column), "") }
    | Literal
    )
    lazy val loc: Parser[Position] = Parser{reader => Success(reader.pos, reader)}    
    lazy val Identifier: Parser[Ident] = loc ~ IdentStart ~ IdentCont.* ^^ {
      case pos ~ s ~ c => Ident(Pos(pos.line, pos.column), Symbol("" + s + c.foldLeft("")(_ + _)))
    }
    lazy val IdentStart: Parser[Char] = crange('a','z') | crange('A','Z') | '_'
    lazy val IdentCont: Parser[Char] = IdentStart | crange('0','9')
    lazy val Literal: Parser[Str] = loc ~ CHAR ^^ {
      case pos ~ c => Str(Pos(pos.line, pos.column), "" + c )
    }
    lazy val CLASS: Parser[CharClass] = {
      (loc <~ chr('[')) ~ opt(chr('^')) ~ ((not(chr(']')) ~> Range).* <~ ']' ~> Spacing) ^^ {
        //negative character class
        case (pos ~ Some(_) ~ rs) => CharClass(Pos(pos.line, pos.column), false, rs)
        //positive character class
        case (pos ~ None ~ rs) => CharClass(Pos(pos.line, pos.column), true, rs)
      }
    }
    lazy val Range: Parser[CharClassElement] = (
      CHAR ~ '-' ~ CHAR ^^ { case f~_~t => CharRange(f, t) }
    | CHAR ^^ { case c => OneChar(c) }
    )
    private val META_CHARS = List('$','|','&','!','?','*','+','(',')','[',']',':',';','=','#','\'','"','\\')
    lazy val META: Parser[Char] = cset(META_CHARS:_*)
    lazy val HEX: Parser[Char] = crange('0','9') | crange('a', 'f')
    lazy val CHAR: Parser[Char] = ( 
      chr('\\') ~ cset('n','r','t','f') ^^ { case _ ~ c => escape(c) }
    | chr('\\') ~> chr('u') ~> (HEX ~ HEX ~ HEX ~ HEX) ^^ {
        case u1 ~ u2 ~ u3 ~ u4 => Integer.parseInt("" + u1 + u2 + u3 + u4, 16).toChar
      }
    | chr('\\') ~ META ^^ { case _ ~ c => c }
    | chr('\\') ~ crange('0','2') ~ crange('0','7') ~ crange('0','7') ^^ { 
        case _ ~ a ~ b ~ c => Integer.parseInt("" + a + b + c, 8).toChar
      }
    | chr('\\') ~ crange('0','7') ~ opt(crange('0','7')) ^^ {
        case _ ~ a ~ Some(b) => Integer.parseInt("" + a + b, 8).toChar
        case _ ~ a ~ _ => Integer.parseInt("" + a, 8).toChar
      }
    | not(META, " meta character " + META_CHARS.mkString("[",",","]") + " is not expected") ~>  any ^^ { case c => c}
    )
    lazy val DOLLAR = chr('$')
    lazy val LT = chr('<')
    lazy val GT = chr('>')
    lazy val COLON = chr(':')
    lazy val SEMI_COLON = chr(';')
    lazy val EQ = chr('=')
    lazy val BAR = chr('|')
    lazy val AND = chr('&')
    lazy val NOT = chr('!')
    lazy val QUESTION = chr('?')
    lazy val STAR = chr('*')
    lazy val PLUS = chr('+')
    lazy val OPEN = chr('(')
    lazy val CLOSE = chr(')')
    lazy val DOT = chr('.')
    lazy val Spacing = (Space | Comment).*
    lazy val Comment = (
      chr('/') ~ chr('/') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
    )
    lazy val Space = chr(' ') | chr('\t') | EndOfLine
    lazy val EndOfLine = chr('\r') ~ chr('\n') | chr('\n') | chr('\r')
    lazy val EndOfFile = not(any)
  }
  
  def parse(fileName: String, content: java.io.Reader): Grammar = {
    ParserCore.GRAMMER(input.StreamReader(content)) match {
      case ParserCore.Success(node, _) => node
      case ParserCore.Failure(msg, rest) => 
        val pos = rest.pos
        throw new ParseException(Pos(pos.line, pos.column), msg)
      case ParserCore.Error(msg, rest) =>
        val pos = rest.pos
        throw new ParseException(Pos(pos.line, pos.column), msg)        
    }
  }
  
  def parse(pattern: String): Grammar = {
    parse("", new StringReader(pattern))
  }
  
  def main(args: Array[String]) {
    val g = parse(args(0), new FileReader(args(0)))
    println(g)
  }
}
