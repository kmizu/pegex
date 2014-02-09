package com
package github
package kmizu
package pegex

import scala.collection.mutable.Stack
import scala.collection.mutable

/**
  * This class represents parsing machines for PEG.
  * The definitions of this parsing machine's instruction is included 
  * in object Insns.
  * @author Kota Mizushima */
class PegVirtualMachine(instructions: List[Instructions.Instruction]) extends AnyRef with Parser {
  private case class Frame(
    startPc: Int, nextPc: Int, startCursor: Int, 
    bindings: Map[Symbol, (StartPos, EndPos)]
  )
  private class MatchingFailureException(offset: Int) extends Exception
  private type Cursor = Int
  private type StartPos = Int
  private type EndPos = Int
  private type Pc = Int
  private type PcStack = IntStack
  private type CursorStack = IntStack
  
  private[this] val insns = instructions.toArray  
  private[this] var startPc = 0
  private[this] var pc = 0
  private[this] var startCursor = 0
  private[this] var bindings: Map[Symbol, (StartPos, EndPos)] = null
  private[this] val callStack = new Stack[Frame]
  private[this] var choicesPc = new PcStack
  private[this] var choicesCursor = new CursorStack
  private[this] var cursor = 0
  private[this] val saved = new Stack[(PcStack, CursorStack)]
  private[this] var input = Array[Char]()
  
  private def isEnd = cursor == input.length
  private def isEnd(pos: Int) = pos >= input.length
  @inline
  private def rewind {
    if(choicesPc.isEmpty) {
      if(!callStack.isEmpty) {
        val Frame(pstartPc, pnextPc, pstartCursor, pbindings) = callStack.pop
        startPc = pstartPc        
        pc = pnextPc
        startCursor = pstartCursor
        bindings = pbindings
        val (pcs, cursors) = saved.pop
        choicesPc = pcs
        choicesCursor = cursors
        rewind
      }else {
        throw new MatchingFailureException(-1)
      }
    }else {
      pc = choicesPc.pop
      cursor = choicesCursor.pop
    }
  }
  private type Handler = (Instructions.Instruction => Any)
  import Instructions.Instruction._
  private[this] val tag2handler: Map[Int, Handler] = Map(
    OP_RETURN -> {(insn) =>
      if(saved.isEmpty) {
        cursor
      }else {
        val Frame(pstartPc, pnextPc, pstartCursor, pbindings) = callStack.pop()
        startPc = pstartPc
        pc = pnextPc
        startCursor = pstartCursor
        bindings = pbindings
        val (pcs, cursors) = saved.pop()
        choicesPc = pcs
        choicesCursor = cursors
        null
      }
    },
    OP_SET_RESULT -> {insn =>
      val Instructions.OpSetResult(_, name) = insn
      bindings = bindings + (name -> (startCursor, cursor))
      pc += 1
      null
    },
    OP_SET_START -> {insn =>
      val Instructions.OpSetStart(_) = insn
      startCursor = cursor
      pc += 1
      null
    },
    OP_CHOICE -> {insn =>
      val Instructions.OpChoice(_, relativeAddr) = insn
      choicesPc.push(pc + relativeAddr)
      choicesCursor.push(cursor)
      pc += 1
      null
    },
    OP_COMMIT -> {insn =>
      val Instructions.OpCommit(_, relativeAddr) = insn
      choicesPc.pop
      choicesCursor.pop
      pc += relativeAddr
      null
    },
    OP_CALL -> {insn =>
      val Instructions.OpCall(_, _, relativeAddr) = insn
      callStack.push(Frame(startPc, pc + 1, startCursor, bindings))
      saved.push((choicesPc, choicesCursor))
      choicesPc = new IntStack
      choicesCursor = new IntStack
      startPc = pc + relativeAddr
      pc = startPc
      bindings = Map.empty
      startCursor = cursor
      null
    },
    OP_JUMP -> {insn =>
      val Instructions.OpJump(_, relativeAddr) = insn
      pc += relativeAddr
      null
    },
    OP_ANY -> {insn =>
      val Instructions.OpAny(_) = insn
      if(isEnd) {
        rewind
      }else {
        cursor += 1
        pc += 1
      }    
      null
    },
    OP_CHAR -> {insn =>
      val Instructions.OpChar(_, c) = insn
      if(isEnd || input(cursor) != c) {
        rewind
      }else {
        cursor += 1
        pc += 1
      }
      null
    },
    OP_STRING -> {insn =>
      val Instructions.OpString(_, str) = insn
      val len = str.length
      if(isEnd(cursor + len - 1)){
        rewind
        null
      }else {
        var i = 0
        while(i < len && str(i) == input(cursor + i)) i += 1
        if(i < len) rewind
        else {
          cursor += len
          pc += 1
        }
        null
      }
    },
    OP_CHAR_CLASS -> {insn =>
      val Instructions.OpCharClass(_, set) = insn
      if(isEnd || !set(input(cursor))) {
        rewind
      }else {
        cursor += 1
        pc += 1
      }
      null
    },
    OP_FAIL -> {insn =>
      rewind
      null
    },
    OP_BACKREF -> {insn =>
      val Instructions.OpBackref(_, name) = insn
      val (start, end) = bindings(name)
      def matches(): Boolean = {
        (0 until (end - start)).forall{i =>
          (!isEnd(cursor + i)) && input(start + i) == input(cursor + i)
        }
      }
      if(!matches()) {
        rewind
      }else {
        cursor += (end - start)
        pc += 1
      }
      null
    },
    OP_CALL_LABEL -> {insn => sys.error("must not reach here!")}
  )
  private[this] val handlers: Array[Handler] = {
    val tmp = new Array[Handler](tag2handler.size)
    for(i <- 0 until tmp.size) tmp(i) = tag2handler(i)
    tmp
  }  
  private def eval(inputStr: String): Int = {
    callStack.clear()
    choicesPc.clear()
    choicesCursor.clear()
    saved.clear()
    startPc = 0
    pc = 0
    startCursor = 0
    cursor = 0
    bindings = Map.empty
    input = inputStr.toCharArray
    var tmpResult: Any = null
    do {
      val insn = insns(pc)
      tmpResult = handlers(insn.tag)(insn)
    }while(tmpResult == null)
    tmpResult.asInstanceOf[Int]
  }
  
  def parse(inputStr: String): MatchResult = {
    try {
      eval(inputStr)
      val result = Some(inputStr.substring(0, cursor))
      MatchResult(
        result, bindings.foldLeft(Map[Symbol, String]()){case (m, (k, v)) =>
          m + (k -> inputStr.substring(v._1, v._2))        
        }
      )
    }catch{
      case e:MatchingFailureException => MatchResult(None, Map.empty)
    }
  }
}
