package com.nurun.icfp_2014.gcccode

import com.nurun.icfp_2014.ir._
import scala.language.implicitConversions

/**
 * Created by dana.harrington on 2014-07-26.
 */
case class GenCode(main: Seq[GCCCode], branches: Seq[LabelledGCC] = Seq()) {
  def ++(gc: GenCode): GenCode = {
    GenCode(main ++ gc.main, branches ++ gc.branches)
  }

  def :+(gc: GCCCode) = GenCode(main :+ gc, branches)

  def toCode(label: String): Seq[LabelledGCC] = {
    main.labelled(label) ++ branches
  }

  def toCode(): Seq[LabelledGCC] = main.map(LabelledGCC(_, None)) ++ branches

  def addBranches(b: Seq[LabelledGCC]) = this.copy(branches = this.branches ++ b)
}

object CodeGen {

  private var freshNames: Iterator[String] = Iterator.from(1).map(idx => s"l$idx")
  private def freshName = freshNames.next()

  def codegen(p: Program): Seq[LabelledGCC] = {
    // Generate code for definitions
    val defs = p.definitions.flatMap(codegen)
    // Generate code for main
    val main = codegen(Map.empty, p.main)
    // Add def code after any main code
    val genCode = main.copy(branches = main.branches ++ defs) :+ STOP
    // Assemble into sequence of labelled GCC code ops
    genCode.toCode()
  }

  def codegen(d: Def): Seq[LabelledGCC] = {

    val debruijn = d.args.zipWithIndex.toMap.mapValues(i => (0,i))
    val generatedCode = codegen(debruijn, d.body)
    (generatedCode :+ RTN).toCode(d.name)
  }

  implicit def seqToGenCode(s: Seq[GCCCode]): GenCode = GenCode(s)
  implicit def gcToLabelledGc(gc: GCCCode): LabelledGCC = LabelledGCC(gc, None)

  // compile ir with a mapping of symbol name to environment offsets
  def codegen(debruijn: Map[String, (Int,Int)], ir: IR): GenCode = {

    ir match {
      case Abs(args, expr) =>
        val localScope = args.zipWithIndex.toMap.mapValues(i => (0,i))
        val parentScope = debruijn.mapValues{ case(fp, idx) => (fp+1, idx)}
        codegen(parentScope ++ localScope, expr) :+ RTN

      case App(Prim(op), xs) =>
        val evalArgs = xs.map(codegen(debruijn, _))
          .reduceOption(_ ++ _)
          .getOrElse(GenCode(Seq()))
        evalArgs :+ op

      case App(Var(f), xs) =>
        val evalArgs = xs.map(codegen(debruijn, _))
          .reduceOption(_ ++ _)
          .getOrElse(GenCode(Seq()))
        evalArgs ++ Seq(LDF(AddressLabel(f)), AP(xs.length))

      case Constant(c) =>
        Seq(LDC(c))

      case Prim(op) =>
        Seq(op)

      case Var(v) =>
        debruijn.get(v) match {
            // var from env
          case Some((fp, idx)) =>
            Seq(LD(fp, idx))
            // top level def
          case None =>
            Seq(LDF(AddressLabel(v)))
        }

      case If(pred, thn, els) =>
        /* Generate labelled branches for 'then' and 'else', each ending with JOIN
         * Evaluate predicate
         * Invoke SEL to jump to the appropriate branch
         */
        val thenLabel = freshName
        val elseLabel = freshName
        val selOp = SEL(AddressLabel(thenLabel), AddressLabel(elseLabel))
        val predCode = codegen(debruijn, pred) :+ selOp
        val thenCode = codegen(debruijn, thn) :+ JOIN
        val elseCode = codegen(debruijn, els) :+ JOIN

        predCode.addBranches(thenCode.toCode(thenLabel))
          .addBranches(elseCode.toCode(elseLabel))


    }
  }
}