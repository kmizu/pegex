package com
package github
package kmizu
package pegex

/** This object provides types representing ASTs of extended PEG.
  * It is used as namespace.
  * @author Kota Mizushima
  */
object AstNode {
  type ==>[-A, +B] = PartialFunction[A, B]
  
  val DUMMY_POS = Pos(-1, -1)
  
  /** A trait for types that has position. */
  trait HasPosition { def pos: Pos }
  /** This class represents position in a source file.
    * @param line line number (0-origin)
    * @param column column number (0-origin) */
  case class Pos (line: Int, column: Int)
  /** This class represents an AST of PEG grammar.
    * @param pos position in source file
    * @param start the start symbol.  A parser start to parse from this symbol
    * @param rules the list of rules constituting PEG grammar */
  case class Grammar(pos: Pos, start: Symbol, rules: List[Rule]) extends HasPosition {
    def +(newRule: Rule): Grammar = Grammar(pos, start, rules = newRule::rules)
  }
  /** This class represents an AST of rule in PEG grammar.
    * @param pos position in source file
    * @param name the name of this rule.  It is referred in body
    * @param body the parsing expression which this rule represents */
  case class Rule(pos: Pos, name: Symbol, body: Expression, action: Any ==> Any = { case a => a }) extends HasPosition
  /** This trait represents common super-type of parsing expression AST. */
  sealed trait Expression extends HasPosition
  /** This class represents an AST of sequence (e1 e2).
    * @param pos position in source file
    * @param lhs e1
    * @param rhs e2 */
  case class Sequence(pos: Pos, lhs: Expression, rhs: Expression) extends Expression
  /** This class represents an AST of unordered choice (e1 | e2).
    * @param pos position in source file
    * @param lhs e1
    * @param rhs e2 */
  case class Choice(pos: Pos, lhs: Expression, rhs: Expression) extends Expression
  /** This class represents an AST of repetition e*.
    * @param pos position in source file
    * @param body e */
  case class Repeat0(pos: Pos, body: Expression) extends Expression
  /** This class represents an AST of one-or-more repetition e+.
    * @param pos position in source file
    * @param body e */
  case class Repeat1(pos: Pos, body: Expression) extends Expression
  /** This class represents an AST of zero-or-one occurrence e?.
    * @param pos position in source file
    * @param body e */
  case class Optional(pos: Pos, body: Expression) extends Expression
  /** This class represents an AST of and-predicate &(e).
    * @param pos position in source file
    * @param body e */
  case class AndPredicate(pos: Pos, body: Expression) extends Expression
  /** This class represents an AST of not-predicate !(e).
    * @param pos position in source file
    * @param body e */
  case class NotPredicate(pos: Pos, body: Expression) extends Expression
  /** This class represents an AST of string literal "...".
    * @param pos position in source file
    * @param target literal */
  case class StringLiteral(pos: Pos, target: String) extends Expression
  /** This class represents an AST of wild-card character ..
    * @param pos position in source file */
  case class Wildcard(pos: Pos) extends Expression
  /** This class represents an AST of character class [...].
    * @param pos position in source file
    * @param elems the list of element constituting character class. */
  case class CharacterClass(pos: Pos, positive: Boolean, elems: List[CharacterClassElement]) extends Expression
  /** This class represents an AST of character set,
   *  which is created from CharSet.
   */
  case class CharacterSet(pos: Pos, positive: Boolean, elems: Set[Char]) extends Expression
  /** This class represents an AST of binder,
   *  which binds the parsing result of exp to name.
   *  @param pos position in source file
   *  @param name name of parsing result of e
   *  @param exp expression evaluated
   */
  case class Binder(pos: Pos, name: Symbol, exp: Expression) extends Expression
  /** This class represents an AST of identifier.
    * An identifier is used as reference of nonterminal.
    * @param pos position in source file
    * @param name the name of identifier */
  case class Identifier(pos: Pos, name: Symbol) extends Expression
  /** This class represents an AST of backward-reference.
    * @param pos position in source file
    * @param name the name of backward-reference */
  case class Backreference(pos: Pos, name: Symbol) extends Expression
  /** This trait represents common super-type of element in character class. */
  sealed trait CharacterClassElement
  /** An element of character class representing one character. */
  case class OneCharacter(ch: Char) extends CharacterClassElement
  /** An element of character class representing characters in this range.
    * @param from start of the range
    * @param to end of the range */
  case class CharacterRange(from: Char, to: Char) extends CharacterClassElement
}
