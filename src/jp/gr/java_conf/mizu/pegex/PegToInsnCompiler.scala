package jp.gr.java_conf.mizu.pegex
import PegParser._

/**
  * This object provides a compiler that compile ASTs of PEG
  * to instructions of parsing machine.
  * @author Kota Mizushima */
object PegToInsnCompiler {
  def compile(grammar :Ast.Grammar): List[Insns.Insn] = {
    val insnsList = grammar.rules.map({r => 
      (r.name, translate(r.body):::List(Insns.OpReturn(r.pos.line)))
    })
    val (i, m) = insnsList.foldLeft((0, Map[Symbol, Int]())){
      case ((i, m), (name, insns)) => (i + insns.size, m(name) = i)
    }
    val insns = insnsList.flatMap{ case (name, insns) => insns }
    insns.zipWithIndex.map{
      case (Insns.OpCallLabel(p, name), i) => Insns.OpCall(p, name, m(name) - i)
      case (otherInsn, i) => otherInsn
    }
  }
  private[this] def translate(e: Ast.Exp): List[Insns.Insn] = e match {
    case Ast.Str(p, str) => 
      if(str.length == 0) Nil
      else if(str.length == 1) List(Insns.OpChar(p.line, str(0)))
      else List(Insns.OpString(p.line, str.toCharArray))
    case c@Ast.CharClass(p, positive, elems) =>
      if(!positive) {
        translate(new Ast.Seq(p,
          Ast.NotPred(p, Ast.CharClass(p, true, elems)), Ast.Wildcard(p)
        ))
      }else {
        Insns.OpCharClass(
          p.line,
          elems.foldLeft(Set[Char]()){       
            case (set, Ast.CharRange(f, t)) => (set /: (f to t))((set, c) => set + c)
            case (set, Ast.OneChar(c)) => set + c
          }
        )::Nil
      }
    case Ast.Wildcard(p) => List(Insns.OpAny(p.line))
    case Ast.Ident(p, name, ref) =>
      List(Insns.OpCallLabel(p.line, name)) :::
      (ref match { 
        case None => Nil
        case Some(refname) => List(Insns.OpSet(p.line, refname))
      })
    case Ast.Backref(p, name) =>
      List(Insns.OpBackref(p.line, name))
    case Ast.Seq(p, lhs, rhs) => Nil
      translate(lhs) ::: translate(rhs)
    case Ast.Alt(p, lhs, rhs) => 
      val lres = translate(lhs)
      val rres = translate(rhs)
      Insns.OpChoice(lhs.pos.line, lres.size + 2) ::
      (lres ::: (Insns.OpCommit(rhs.pos.line, rres.size + 1) :: rres))
    case Ast.Rep0(p, body) =>
      val res = translate(body)
      Insns.OpChoice(p.line, res.size + 2) :: res ::: 
      List(Insns.OpCommit(p.line, - (res.size + 1)))
    case Ast.NotPred(p, body) => 
      val res = translate(body)
      Insns.OpChoice(p.line, res.size + 3) :: res ::: Insns.OpCommit(p.line, 1) :: 
      List(Insns.OpFail(p.line))
    case Ast.Rep1(p, body) =>
      translate(Ast.Seq(p, body, Ast.Rep0(p, body)))
    case Ast.AndPred(p, body) => Nil
      translate(Ast.NotPred(p, Ast.NotPred(p, body)))
    case Ast.Opt(p, body) =>
      translate(Ast.Alt(p, body, Ast.Str(p, "")))
  }
}
