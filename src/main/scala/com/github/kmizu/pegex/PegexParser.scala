package com
package github
package kmizu
package pegex

import scala.util.parsing.combinator._
import scala.util.parsing.input.{CharSequenceReader, StreamReader}
import scala.util.parsing.input.Position
import java.io._
import AstNode._
/**
  * This object provides a parser that parses strings in Pegex and translates
  * them into ASTs of PEGEX (which is like PEGs).
  * @author Kota Mizushima
  *
  */
object PegexParser {

  /**
   * This exception is thrown in the case of a parsing failure
   * @param pos the position where the parsing failed
   * @param msg error message
   */
  case class ParseException(pos: Pos, msg: String) extends Exception(pos.line + ", " + pos.column + ":" + msg)
  
  private object ParserCore extends Parsers {
    type Elem = Char
    val StartRuleName: Symbol = 'S
    private val any: Parser[Char] = elem(".", c => c != CharSequenceReader.EofCh)
    private def char(c: Char): Parser[Char] = c
    private def crange(f: Char, t: Char): Parser[Char] = elem("[]", c => f <= c && c <= t)
    private def cset(cs: Char*): Parser[Char] = elem("[]", c => cs.indexWhere(_ == c) >= 0)
    private val escape: Map[Char, Char] = Map(
      'n' -> '\n', 'r' -> '\r', 't' -> '\t', 'f' -> '\f'
    )
    private def not[T](p: => Parser[T], msg: String): Parser[Unit] = {
      not(p) | failure(msg)
    }
    lazy val GRAMMER: Parser[Grammar] = (loc ~ Expression <~ (SEMI_COLON <~ Spacing)) ~ Definition.* <~ EndOfFile ^^ {
      case (pos ~ e) ~ rules => Grammar(Pos(pos.line, pos.column), StartRuleName, Rule(Pos(pos.line, pos.column), StartRuleName, e)::rules)
    }
    lazy val Definition: Parser[Rule] = (Identifier <~ EQ) ~ 
      Expression <~ SEMI_COLON <~ Spacing ^^ {
      case n ~ b => Rule(n.pos, n.name, b)
    }  
    
    lazy val Expression: Parser[Expression] = rep1sep(Sequence, BAR) ^^ { ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => Choice(y.pos, a, y)}
    }
    lazy val Sequence: Parser[Expression]   = Prefix.+ ^^ { ns =>
      val x :: xs = ns; xs.foldLeft(x){(a, y) => AstNode.Sequence(y.pos, a, y)}
    }
    lazy val Prefix: Parser[Expression]     = (
      (loc <~ AND) ~ Suffix ^^ { case pos ~ e => AndPredicate(Pos(pos.line, pos.column), e) }
    | (loc <~ NOT) ~ Suffix ^^ { case pos ~ e => NotPredicate(Pos(pos.line, pos.column), e) }
    | Suffix
    )
    lazy val Suffix: Parser[Expression]     = (
      loc ~ Primary <~ QUESTION ^^ { case pos ~ e => Optional(Pos(pos.line, pos.column), e) }
    | loc ~ Primary <~ STAR ^^ { case pos ~ e => Repeat0(Pos(pos.line, pos.column), e) }
    | loc ~ Primary <~ PLUS ^^ { case pos ~ e => Repeat1(Pos(pos.line, pos.column), e) }
    | Primary
    )
    lazy val Primary: Parser[Expression]    = (
      (char('#') ~> char('(')) ~> Identifier ~
      opt(char(':') ~> char(':') ~> Expression | char(':') ~> Identifier) <~ char(')') ^^ {
        case ident ~ Some(exp) => Binder(ident.pos, ident.name, exp)
        case ident ~ None => ident
      }
    | OPEN ~> Expression <~ CLOSE
    | (char('#') ~> char('#') ~> char('(')) ~> Identifier <~ char(')') ^^ {ident => Backreference(ident.pos, ident.name)}
    | CLASS
    | loc <~ DOLLAR ^^ { case pos => 
        val p = Pos(pos.line, pos.column); NotPredicate(p, Wildcard(p))
      }
    | loc <~ DOT ^^ { case pos => Wildcard(Pos(pos.line, pos.column)) }
    | loc <~ HAT ^^ { case pos => Start(Pos(pos.line, pos.column)) }
    | loc <~ char('_') ^^ { case pos => StringLiteral(Pos(pos.line, pos.column), "") }
    | Literal
    )
    lazy val loc: Parser[Position]          = Parser{reader => Success(reader.pos, reader)}
    lazy val Identifier: Parser[Identifier] = loc ~ IdentStart ~ IdentCont.* ^^ {
      case pos ~ s ~ c => AstNode.Identifier(Pos(pos.line, pos.column), Symbol("" + s + c.foldLeft("")(_ + _)))
    }
    lazy val IdentStart: Parser[Char]       = crange('a','z') | crange('A','Z') | '_'
    lazy val IdentCont: Parser[Char]        = IdentStart | crange('0','9')
    lazy val Literal: Parser[StringLiteral]           = loc ~ CHAR ^^ {
      case pos ~ c => StringLiteral(Pos(pos.line, pos.column), "" + c )
    }
    lazy val CLASS: Parser[CharacterClass]       = {
      (loc <~ char('[')) ~ opt(char('^')) ~ ((not(char(']')) ~> Range).* <~ ']' ~> Spacing) ^^ {
        //negative character class
        case (pos ~ Some(_) ~ rs) => CharacterClass(Pos(pos.line, pos.column), false, rs)
        //positive character class
        case (pos ~ None ~ rs) => CharacterClass(Pos(pos.line, pos.column), true, rs)
      }
    }
    lazy val Range: Parser[CharacterClassElement] = (
      CHAR ~ '-' ~ CHAR ^^ { case f~_~t => CharacterRange(f, t) }
    | CHAR ^^ { case c => OneCharacter(c) }
    )
    private val META_CHARS = List('$','|','&','!','?','*','+','(',')','[',']',':',';','=','#','\'','"','\\')
    lazy val META: Parser[Char] = cset(META_CHARS:_*)
    lazy val HEX: Parser[Char] = crange('0','9') | crange('a', 'f')
    lazy val CHAR: Parser[Char] = ( 
      char('\\') ~> cset('n','r','t','f') ^^ { case c => escape(c) }
    | char('\\') ~> char('u') ~> (HEX ~ HEX ~ HEX ~ HEX) ^^ {
        case u1 ~ u2 ~ u3 ~ u4 => Integer.parseInt("" + u1 + u2 + u3 + u4, 16).toChar
      }
    | char('\\') ~ META ^^ { case _ ~ c => c }
    | char('\\') ~ crange('0','2') ~ crange('0','7') ~ crange('0','7') ^^ {
        case _ ~ a ~ b ~ c => Integer.parseInt("" + a + b + c, 8).toChar
      }
    | char('\\') ~ crange('0','7') ~ opt(crange('0','7')) ^^ {
        case _ ~ a ~ Some(b) => Integer.parseInt("" + a + b, 8).toChar
        case _ ~ a ~ _ => Integer.parseInt("" + a, 8).toChar
      }
    | not(META, " meta character " + META_CHARS.mkString("[",",","]") + " is not expected") ~>  any ^^ { case c => c}
    )
    lazy val DOLLAR = char('$')
    lazy val LT = char('<')
    lazy val GT = char('>')
    lazy val COLON = char(':')
    lazy val SEMI_COLON = char(';')
    lazy val EQ = char('=')
    lazy val BAR = char('|')
    lazy val AND = char('&')
    lazy val NOT = char('!')
    lazy val QUESTION = char('?')
    lazy val STAR = char('*')
    lazy val PLUS = char('+')
    lazy val OPEN = char('(')
    lazy val CLOSE = char(')')
    lazy val DOT = char('.')
    lazy val HAT = char('^')
    lazy val Spacing = (Space | Comment).*
    lazy val Comment = (
      char('/') ~ char('/') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
    )
    lazy val Space = char(' ') | char('\t') | EndOfLine
    lazy val EndOfLine = char('\r') ~ char('\n') | char('\n') | char('\r')
    lazy val EndOfFile = not(any)
  }

  /**
   * Parses a pattern from `content` and returns the `Grammar` instance, which is the parse result.
   * @param fileName
   * @param content
   * @return `Grammar` instance
   */
  def parse(fileName: String, content: java.io.Reader): Grammar = {
    ParserCore.GRAMMER(StreamReader(content)) match {
      case ParserCore.Success(node, _) => node
      case ParserCore.Failure(msg, rest) => 
        val pos = rest.pos
        throw new ParseException(Pos(pos.line, pos.column), msg)
      case ParserCore.Error(msg, rest) =>
        val pos = rest.pos
        throw new ParseException(Pos(pos.line, pos.column), msg)        
    }
  }

  /**
   * Parses a `pattern` and returns the `Grammar` instance, which is the parse result.
   * @param pattern input string
   * @return `Grammar` instance
   */
  def parse(pattern: String): Grammar = {
    parse("", new StringReader(pattern))
  }
  
  def main(args: Array[String]) {
    val g = parse(args(0), new FileReader(args(0)))
    println(g)
  }
}
