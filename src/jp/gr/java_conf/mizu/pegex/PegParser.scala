package jp.gr.java_conf.mizu.pegex
import scala.util.parsing.combinator._
import scala.util.parsing.input
import input._
import java.io._
import Ast._

/**
  * This object provides a parser that parses strings in PEG and translates
  * them into ASTs of PEG.  Pegex's parser is included in PegexParser.
  * @author Kota Mizushima */
object PegParser {
  case class ParseException(pos: Pos, msg: String) extends Exception(pos + msg)
  
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
    lazy val GRAMMER: Parser[Grammar] = (loc <~ Spacing) ~ Definition.+ <~ EndOfFile ^^ {
      case pos ~ rules => Grammar(Pos(pos.line, pos.column), rules.head.name, rules)
    }
    lazy val Definition: Parser[Rule] = (Identifier <~ (LEFTARROW | EQ)) ~ 
      Expression <~ SEMI_COLON ^^ {
      case n ~ b => Rule(n.pos, n.name, b)
    }      
    lazy val Expression: Parser[Exp] = Sequence ~ (SLASH ~> Sequence).* ^^ {
      case x ~ xs => xs.foldLeft(x){(a, y) => Alt(y.pos, a, y)}
    }
    lazy val Sequence: Parser[Exp] = Prefix.+ ^^ {ys => 
      val x::xs = ys
      xs.foldLeft(x){(a, y) => Seq(y.pos, a, y)}
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
      (Identifier <~ COLON) ~ Expression ^^ { case ident ~ exp => 
        Binder(ident.pos, ident.name, exp)
      }
    | Identifier ^^ { case ident => Ident(ident.pos, ident.name) }
    | OPEN ~> Expression <~ CLOSE
    | LT ~> Identifier <~ GT ^^ {ident => Backref(ident.pos, ident.name)}
    | Literal
    | CLASS
    | loc <~ (DOT | UNDER_SCORE) ^^ { case pos => Wildcard(Pos(pos.line, pos.column)) }
    )
    lazy val loc: Parser[Position] = Parser{reader => Success(reader.pos, reader)}    
    lazy val Identifier: Parser[Ident] = loc ~ IdentStart ~ IdentCont.* <~Spacing ^? {
      case pos ~ s ~ c  if !(c.length == 0 && s == '_') => Ident(Pos(pos.line, pos.column), Symbol("" + s + c.foldLeft("")(_ + _)))
    }
    lazy val IdentStart: Parser[Char] = crange('a','z') | crange('A','Z') | '_'
    lazy val IdentCont: Parser[Char] = IdentStart | crange('0','9')
    lazy val Literal: Parser[Str] = loc ~ (
      chr('\'') ~> (not('\'') ~> CHAR).* <~ chr('\'') <~ Spacing
    | chr('"') ~> (not('"') ~> CHAR).* <~ chr('"') <~ Spacing
    ) ^^ {
      case pos ~ cs => Str(Pos(pos.line, pos.column), cs.foldLeft(""){(acc, n) => acc + n})
    }
    lazy val CLASS: Parser[CharClass] = (loc <~ chr('[')) ~ opt(chr('^')) ~ (not(chr(']')) ~> Range).* <~ ']' ~> Spacing ^^ {
      //negative character class
      case (pos ~ Some(_) ~ rs) => CharClass(Pos(pos.line, pos.column), false, rs)
      //positive character class
      case (pos ~ None ~ rs) => CharClass(Pos(pos.line, pos.column), true, rs)      
    }
    lazy val Range: Parser[CharClassElement] = (
      CHAR ~ '-' ~ CHAR ^^ { case f~_~t => CharRange(f, t) }
    | CHAR ^^ { case c => OneChar(c) }
    )
    lazy val META: Parser[Char] = cset('[',']','\'','"','\\','-')
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
    | not('\\') ~ any ^^ { case _ ~ c => c}
    )
    lazy val LT = '<' <~ Spacing
    lazy val GT = '>' <~ Spacing
    lazy val COLON = ':' <~ Spacing
    lazy val SEMI_COLON = ';' <~ Spacing
    lazy val EQ = chr('=') <~ Spacing
    lazy val LEFTARROW = chr('<') ~ '-' <~ Spacing
    lazy val SLASH = '/' <~ Spacing
    lazy val AND = '&' <~ Spacing
    lazy val NOT = '!' <~ Spacing
    lazy val QUESTION = '?' <~ Spacing
    lazy val STAR = '*' <~ Spacing
    lazy val PLUS = '+' <~ Spacing
    lazy val OPEN = '(' <~ Spacing
    lazy val CLOSE = ')' <~ Spacing
    lazy val DOT = '.' <~ Spacing
    lazy val UNDER_SCORE = '_' <~ Spacing
    lazy val Spacing = (Space | Comment).*
    lazy val Comment = (
      chr('#') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
    | chr('/') ~ chr('/') ~ (not(EndOfLine) ~ any).* ~ EndOfLine
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
