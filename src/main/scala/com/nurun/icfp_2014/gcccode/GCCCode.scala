package com.nurun.icfp_2014.gcccode

/**
 * Created by dana.harrington on 2014-07-25.
 */
case class Env(label: Map[String, AddressLiteral])

object Env {
  def empty = Env(Map.empty[String, AddressLiteral])
  
  def build(program: Seq[LabelledGCC]): Env = {
    program.zipWithIndex.foldLeft(Env.empty){ case (env, (op,idx)) =>
      op.label match {
        case Some(label) => env.copy(label = env.label + (label -> AddressLiteral(idx)))
        case None => env
      }
    }
  } 
}

object GCCCode {
  /* Translate symbolic labels to addresses */
  def delabel(ops: Seq[LabelledGCC]): Seq[GCCCode] = {
    val env = Env.build(ops)
    ops.map(_.code).map(_.bind(env))
  }

  implicit class LabellableGCC(code: GCCCode) {
    def labelled(l: String): LabelledGCC = LabelledGCC(code, Some(l))
  }

  implicit class LabellableGCCSeq(code: Seq[GCCCode]) {
    // add label to first op code of a sequence
    def labelled(l: String): List[LabelledGCC] = {
      code.toList match {
        case Nil => Nil
        case h :: tl => h.labelled(l) :: tl.map(LabelledGCC(_, None))
      }
    }
  }

}

// A symbolic or absolute GCC address
sealed trait Address {
  def output: String
  def bind(env: Env): Address // bind label addresses to an address literal
}
case class AddressLiteral(literal: Literal) extends Address {
  def output = literal.toString
  def bind(env: Env) = this
}
case class AddressLabel(label: String) extends Address {
  def output = label
  def bind(env: Env) = env.label.getOrElse(label, this)
}

// A GCC code operation
sealed trait GCCCode {
  def output: String
  def bind(env: Env): GCCCode = this // bind any label address to a literal value from the environment
}
case class LDC(lit: Literal) extends GCCCode {
  def output = s"LDC $lit"
}
case class LD(env_offset: Int, data_offset: Int) extends GCCCode {
  def output = s"LD $env_offset $data_offset"
}
case object ADD extends GCCCode with PrimitiveOp {
  def output = s"ADD"
}
case object SUB extends GCCCode with PrimitiveOp {
  def output = s"SUB"
}
case object MUL extends GCCCode with PrimitiveOp {
  def output = s"MUL"
}
case object DIV extends GCCCode with PrimitiveOp {
  def output = s"DIV"
}
case object CEQ extends GCCCode with PrimitiveOp {
  def output = s"CEQ"
}
case object CGT extends GCCCode with PrimitiveOp {
  def output = s"CGT"
}
case object CGTE extends GCCCode with PrimitiveOp {
  def output = s"CGTE"
}
case object ATOM extends GCCCode with PrimitiveOp {
  def output = s"ATOM"
}
case object CONS extends GCCCode with PrimitiveOp {
  def output = s"CONS"
}
case object CAR extends GCCCode with PrimitiveOp {
  def output = s"CAR"
}
case object CDR extends GCCCode with PrimitiveOp {
  def output = s"CDR"
}
case class SEL(true_address: Address, false_address: Address) extends GCCCode {
  def output = s"SEL ${true_address.output} ${false_address.output}"
  override def bind(env: Env) = SEL(true_address.bind(env), false_address.bind(env))
}
case object JOIN extends GCCCode {
  def output = s"JOIN"
}
case class LDF(address: Address) extends GCCCode {
  def output = s"LDF ${address.output}"
  override def bind(env: Env) = LDF(address.bind(env))
}
case class AP(frame_size: Int) extends GCCCode {
  def output = s"AP $frame_size"
}
case object RTN extends GCCCode {
  def output = s"RTN"
}
case class DUM(frame_size: Int) extends GCCCode {
  def output = s"DUM $frame_size"
}
case class RAP(arguments: Int) extends GCCCode {
  def output = s"RAP $arguments"
}
case object STOP extends GCCCode {
  def output = s"STOP"
}


// Primative ops are referenced directly in the source language and can be applied directly
trait PrimitiveOp extends GCCCode


// A GCC operation with an optional label
case class LabelledGCC(code: GCCCode, label: Option[String]) {
  def output: String = label.map(_ + ":\n").getOrElse("") ++ "  " ++ code.output
}
