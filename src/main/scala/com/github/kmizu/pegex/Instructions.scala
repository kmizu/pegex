package com
package github
package kmizu
package pegex

import scala.collection.immutable.Set

/** This object provides types representing instructions
  * of parsing virtual machine.  It is used as namespace.
  * @author Kota Mizushima */
object Instructions {
  sealed trait Instruction {
    def line: Int
    val tag: Int
  }
  object Instruction {
    private[this] var count = 0
    private[this] def countUp = { val old = count; count += 1; old }
    val 
    OP_CHAR_CLASS, OP_CHAR, OP_STRING, OP_ANY, OP_CHOICE,
    OP_JUMP, OP_CALL, OP_RETURN, OP_COMMIT, OP_FAIL,
    OP_CALL_LABEL, OP_SET_RESULT, OP_SET_START, OP_BACKREF = countUp
  }
  case class OpString(line: Int, str: Array[Char]) extends Instruction {
    override def toString = "OpString \"" + str.mkString + "\""
    val tag = Instruction.OP_STRING
  }
  case class OpCharClass(line: Int, chars: Set[Char]) extends Instruction {
    override def toString = "OpCharClass " + chars + ""
    val tag = Instruction.OP_CHAR_CLASS
  }
  case class OpChar(line: Int, ch: Char) extends Instruction {
    override def toString = "OpChar " + ch + ""
    val tag = Instruction.OP_CHAR
  }
  case class OpAny(line: Int) extends Instruction {
    override def toString = "OpAny"
    val tag = Instruction.OP_ANY
  }
  case class OpChoice(line: Int, relativeAddr: Int) extends Instruction {
    override def toString = "OpChoice " + relativeAddr
    val tag = Instruction.OP_CHOICE
  }
  case class OpJump(line: Int, relativeAddr: Int) extends Instruction {
    override def toString = "OpChoice " + relativeAddr
    val tag = Instruction.OP_JUMP
  }
  case class OpCall(line: Int, label: Symbol, relativeAddr: Int) extends Instruction {
    override def toString = "OpCall " + relativeAddr + "(" + label + ")"
    val tag = Instruction.OP_CALL
  }
  case class OpReturn(line: Int) extends Instruction {
    override def toString = "OpReturn"
    val tag = Instruction.OP_RETURN
  }
  case class OpCommit(line: Int, relativeAddr: Int) extends Instruction {
    override def toString = "OpCommit " + relativeAddr
    val tag = Instruction.OP_COMMIT
  }
  case class OpFail(line: Int) extends Instruction {
    override def toString = "OpFail"
    val tag = Instruction.OP_FAIL
  }
  case class OpCallLabel(line: Int, label: Symbol) extends Instruction {
    override def toString = "OpCallLabel " + label
    val tag = Instruction.OP_CALL_LABEL
  }
  case class OpSetResult(line: Int, name: Symbol) extends Instruction {
    override def toString = "OpSet " + name
    val tag = Instruction.OP_SET_RESULT
  }
  case class OpSetStart(line: Int) extends Instruction {
    override def toString = "OpSetStart"
    val tag = Instruction.OP_SET_START
  }

  case class OpBackref(line: Int, name: Symbol) extends Instruction {
    override def toString = "OpBackref " + name
    val tag = Instruction.OP_BACKREF
  }
}
