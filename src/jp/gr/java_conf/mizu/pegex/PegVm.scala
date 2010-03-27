package jp.gr.java_conf.mizu.pegex
import scala.collection.mutable.Stack
import scala.collection.mutable

/**
  * This class represents parsing machines for PEG.
  * The definitions of this parsing machine's instruction is included 
  * in object Insns.
  * @author Kota Mizushima */
class PegVm(instructions: List[Insns.Insn]) extends AnyRef with Parser {
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
  
  private[this] val insns: Array[Insns.Insn] = instructions.toArray  
  private[this] var startPc: Int = 0
  private[this] var pc: Int = 0
  private[this] var startCursor: Int = 0
  private[this] var bindings: Map[Symbol, (StartPos, EndPos)] = null
  private[this] var callStack: Stack[Frame] = new Stack
  private[this] var choicesPc: PcStack = new IntStack
  private[this] var choicesCursor: CursorStack = new IntStack
  private[this] var cursor: Int = 0  
  private[this] var saved: Stack[(PcStack, CursorStack)] = new Stack
  private[this] var input: Array[Char] = null
  private[this] var result: (StartPos, EndPos) = null
  
  private def isEnd: Boolean = cursor == input.length
  private def isEnd(pos: Int): Boolean = pos >= input.length
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
  private type Handler = (Insns.Insn => Any)
  import Insns.Insn._
  private[this] val tag2handler: Map[Int, Handler] = Map(
    OP_RETURN -> {(insn) =>
      if(saved.isEmpty) {
        cursor
      }else {
        val Frame(pstartPc, pnextPc, pstartCursor, pbindings) = callStack.pop
        result = (startCursor, cursor)
        startPc = pstartPc
        pc = pnextPc
        startCursor = pstartCursor
        bindings = pbindings
        val (pcs, cursors) = saved.pop
        choicesPc = pcs
        choicesCursor = cursors
        null
      }
    },
    OP_SET -> {insn =>
      val Insns.OpSet(_, name) = insn
      bindings = bindings + (name -> result)
      pc += 1
      null
    },
    OP_CHOICE -> {insn =>
      val Insns.OpChoice(_, relativeAddr) = insn
      choicesPc.push(pc + relativeAddr)
      choicesCursor.push(cursor)
      pc += 1
      null
    },
    OP_COMMIT -> {insn =>
      val Insns.OpCommit(_, relativeAddr) = insn
      choicesPc.pop
      choicesCursor.pop
      pc += relativeAddr
      null
    },
    OP_CALL -> {insn =>
      val Insns.OpCall(_, _, relativeAddr) = insn           
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
      val Insns.OpJump(_, relativeAddr) = insn
      pc += relativeAddr
      null
    },
    OP_ANY -> {insn =>
      val Insns.OpAny(_) = insn
      if(isEnd) {
        rewind
      }else {
        cursor += 1
        pc += 1
      }    
      null
    },
    OP_CHAR -> {insn =>
      val Insns.OpChar(_, c) = insn
      if(isEnd || input(cursor) != c) {
        rewind
      }else {
        cursor += 1
        pc += 1
      }
      null
    },
    OP_STRING -> {insn =>
      val Insns.OpString(_, str) = insn
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
      val Insns.OpCharClass(_, set) = insn
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
      val Insns.OpBackref(_, name) = insn
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
    OP_CALL_LABEL -> {insn => error("must not reach here!")}
  )
  private[this] val handlers: Array[Handler] = {
    val tmp = new Array[Handler](tag2handler.size)
    for(i <- 0 until tmp.size) tmp(i) = tag2handler(i)
    tmp
  }  
  private def eval(inputStr: String): Int = {
    callStack.clear()
    choicesPc.clear
    choicesCursor.clear
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
  
  def parseWithGroup(inputStr: String): MatchResult = {
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
