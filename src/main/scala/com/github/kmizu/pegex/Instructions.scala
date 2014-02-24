package com
package github
package kmizu
package pegex

import scala.collection.immutable.Set

/** This object provides types representing instructions
  * of parsing virtual machine.  It is used as namespace.
  * @author Kota Mizushima
  */
object Instructions {
  sealed abstract class Instruction(val tag: Int) {
    def line: Int
  }
  object Instruction {
    private[this] var count = 0
    private[this] def countUp = { val old = count; count += 1; old }
    val 
    OP_CHAR_CLASS, OP_CHAR, OP_STRING, OP_ANY, OP_CHOICE,
    OP_JUMP, OP_CALL, OP_RETURN, OP_COMMIT, OP_FAIL,
    OP_CALL_LABEL, OP_SET_RESULT, OP_SET_START, OP_BACKREF = countUp
  }
  case class OpString(line: Int, str: Array[Char]) extends Instruction(Instruction.OP_STRING) {
    override def toString = "OpString \"" + str.mkString + "\""
  }
  case class OpCharClass(line: Int, chars: Set[Char]) extends Instruction(Instruction.OP_CHAR_CLASS) {
    override def toString = "OpCharClass " + chars + ""
  }
  case class OpChar(line: Int, ch: Char) extends Instruction(Instruction.OP_CHAR) {
    override def toString = "OpChar " + ch + ""
  }
  case class OpAny(line: Int) extends Instruction(Instruction.OP_ANY) {
    override def toString = "OpAny"
  }
  case class OpChoice(line: Int, relativeAddr: Int) extends Instruction(Instruction.OP_CHOICE) {
    override def toString = "OpChoice " + relativeAddr
  }
  case class OpJump(line: Int, relativeAddr: Int) extends Instruction(Instruction.OP_JUMP) {
    override def toString = "OpChoice " + relativeAddr
  }
  case class OpCall(line: Int, label: Symbol, relativeAddr: Int) extends Instruction(Instruction.OP_CALL) {
    override def toString = "OpCall " + relativeAddr + "(" + label + ")"
  }
  case class OpReturn(line: Int) extends Instruction(Instruction.OP_RETURN) {
    override def toString = "OpReturn"
  }
  case class OpCommit(line: Int, relativeAddr: Int) extends Instruction(Instruction.OP_COMMIT) {
    override def toString = "OpCommit " + relativeAddr
  }
  case class OpFail(line: Int) extends Instruction(Instruction.OP_FAIL) {
    override def toString = "OpFail"
  }
  case class OpCallLabel(line: Int, label: Symbol) extends Instruction(Instruction.OP_CALL_LABEL) {
    override def toString = "OpCallLabel " + label
  }
  case class OpSetResult(line: Int, name: Symbol) extends Instruction(Instruction.OP_SET_RESULT) {
    override def toString = "OpSet " + name
  }
  case class OpSetStart(line: Int) extends Instruction(Instruction.OP_SET_START) {
    override def toString = "OpSetStart"
  }
  case class OpBackref(line: Int, name: Symbol) extends Instruction(Instruction.OP_BACKREF) {
    override def toString = "OpBackref " + name
  }
}
