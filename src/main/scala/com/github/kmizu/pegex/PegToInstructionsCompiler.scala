package com
package github
package kmizu
package pegex

import PegParser._

/**
  * This object provides a compiler that compile ASTs of PEG
  * to instructions of parsing machine.
  * @author Kota Mizushima */
object PegToInstructionsCompiler {
  def compile(grammar :Ast.Grammar): Seq[Instructions.Instruction] = {
    val insnsList = grammar.rules.toVector.map({r =>
      (r.name, translate(r.body) ++: Vector(Instructions.OpReturn(r.pos.line)))
    })
    val (i, m) = insnsList.foldLeft((0, Map[Symbol, Int]())){
      case ((i, m), (name, insns)) => (i + insns.size, m.updated(name,i))
    }
    val insns = insnsList.flatMap{ case (name, insns) => insns }
    insns.zipWithIndex.map{
      case (Instructions.OpCallLabel(p, name), i) => Instructions.OpCall(p, name, m(name) - i)
      case (otherInsn, i) => otherInsn
    }
  }
  private[this] def translate(e: Ast.Exp): Seq[Instructions.Instruction] = e match {
    case Ast.Str(p, str) => 
      if(str.length == 0) Vector.empty
      else if(str.length == 1) Vector(Instructions.OpChar(p.line, str(0)))
      else List(Instructions.OpString(p.line, str.toCharArray))
    case c@Ast.CharClass(p, positive, elems) =>
      if(!positive) {
        translate(new Ast.Seq(p,
          Ast.NotPred(p, Ast.CharClass(p, true, elems)), Ast.Wildcard(p)
        ))
      }else {
        Vector(Instructions.OpCharClass(
          p.line,
          elems.foldLeft(Set[Char]()){       
            case (set, Ast.CharRange(f, t)) => (set /: (f to t))((set, c) => set + c)
            case (set, Ast.OneChar(c)) => set + c
          }
        ))
      }
    case Ast.Wildcard(p) => Vector(Instructions.OpAny(p.line))
    case Ast.Ident(p, name) =>
      Vector(Instructions.OpCallLabel(p.line, name))
    case Ast.Binder(p, name, exp) =>
      Vector(Instructions.OpSetStart(p.line)) ++: translate(exp) ++: Vector(Instructions.OpSetResult(p.line, name))
    case Ast.Backref(p, name) =>
      Vector(Instructions.OpBackref(p.line, name))
    case Ast.Seq(p, lhs, rhs) => Nil
      translate(lhs) ++: translate(rhs)
    case Ast.Alt(p, lhs, rhs) => 
      val lres = translate(lhs)
      val rres = translate(rhs)
      Instructions.OpChoice(lhs.pos.line, lres.size + 2) +: (lres ++: (Instructions.OpCommit(rhs.pos.line, rres.size + 1) +: rres))
    case Ast.Rep0(p, body) =>
      val res = translate(body)
      Instructions.OpChoice(p.line, res.size + 2) +: res ++: Vector(Instructions.OpCommit(p.line, - (res.size + 1)))
    case Ast.NotPred(p, body) => 
      val res = translate(body)
      (Instructions.OpChoice(p.line, res.size + 3) +: res) ++: Instructions.OpCommit(p.line, 1) +: Vector(Instructions.OpFail(p.line))
    case Ast.Rep1(p, body) =>
      translate(Ast.Seq(p, body, Ast.Rep0(p, body)))
    case Ast.AndPred(p, body) => Nil
      translate(Ast.NotPred(p, Ast.NotPred(p, body)))
    case Ast.Opt(p, body) =>
      translate(Ast.Alt(p, body, Ast.Str(p, "")))
    case Ast.CharSet(_, _, _) => sys.error("should not reach here")
  }
}
