package org.onion_lang.pegex
import scala.collection.immutable.Set

/** This object provides types representing instructions
  * of optimized parsing virtual machine.  It is used as 
  * namespace.
  * @author Kota Mizushima */
object FastInsns {
  sealed trait Insn {
    def line: Int
    val tag: Int
  }
  sealed trait JumpInsn {
    val to: Either[Symbol, Int]
    lazy val toAddr: Int = to match { 
      case Right(addr) => addr 
      case _ => error("to must be Right(addr)")
    }
    lazy val toLabel: Symbol = to match { 
      case Left(label) => label 
      case _ => error("to must be Left(label)")
    }
  }
  type JumpTarget = Either[Symbol, Int]
  object Insn {
    private[this] var count = 0
    private[this] def countUp = { val old = count; count += 1; old }
    val 
    OP_CHAR_CLASS, OP_CHAR, OP_STRING, OP_ANY, OP_CHOICE,
    OP_JUMP, OP_CALL, OP_RETURN, OP_COMMIT, OP_FAIL,
    OP_CALL_LABEL, OP_SET, OP_BACKREF = countUp
  }
  case class OpString(line: Int, str: Array[Char], to: JumpTarget) extends Insn with JumpInsn {
    override def toString = "OpString \"" + str.mkString + "\""
    val tag = Insn.OP_STRING 
  }
  case class OpCharClass(line: Int, chars: Set[Char], to: JumpTarget) extends Insn {
    override def toString = "OpCharClass " + chars + ""
    val tag = Insn.OP_CHAR_CLASS
  }
  case class OpChar(line: Int, ch: Char, fail: Symbol, to: JumpTarget) extends Insn {
    override def toString = "OpChar " + ch + ""
    val tag = Insn.OP_CHAR 
  }
  case class OpAny(line: Int, to: JumpTarget) extends Insn {
    override def toString = "OpAny"
    val tag = Insn.OP_ANY
  }
  case class OpJump(line: Int, to: JumpTarget) extends Insn {
    override def toString = "OpJump to: " + (to match {
      case Left(label) => label.name
      case Right(addr) => addr.toString
    })
    val tag = Insn.OP_JUMP
  }
  case class OpCall(line: Int, label: Symbol, relativeAddr: Int) extends Insn {
    override def toString = "OpCall " + relativeAddr + "(" + label + ")"
    val tag = Insn.OP_CALL
  }
  case class OpReturn(line: Int) extends Insn {
    override def toString = "OpReturn"
    val tag = Insn.OP_RETURN
  }
  case class OpCommit(line: Int, relativeAddr: Int) extends Insn {
    override def toString = "OpCommit " + relativeAddr
    val tag = Insn.OP_COMMIT 
  }
  case class OpFail(line: Int) extends Insn {
    override def toString = "OpFail"
    val tag = Insn.OP_FAIL
  }
  case class OpCallLabel(line: Int, label: Symbol) extends Insn {
    override def toString = "OpCallLabel " + label
    val tag = Insn.OP_CALL_LABEL 
  }
  case class OpSet(line: Int, name: Symbol) extends Insn {
    override def toString = "OpSet " + name
    val tag = Insn.OP_SET
  }
  case class OpBackref(line: Int, name: Symbol) extends Insn {
    override def toString = "OpBackref " + name
    val tag = Insn.OP_BACKREF
  }
}
