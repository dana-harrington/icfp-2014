package com.nurun.icfp_2014

import com.nurun.icfp_2014.gcccode.{Example, Backend, Env, GCCCode}

import scala.io.Source

object Compile {
  def main(args: Array[String]): Unit = {
    args.headOption.foreach { file =>
      // parse
      // compile
      // labelled output
      // delabelled output

      //srcText foreach println

      //GCCCode.delabel(ExampleLabelledGCC.ex1).map(_.output).foreach(println)
      val labelled = Backend.compile(Example.ex1)
      val delabelled = GCCCode.delabel(labelled)
      val output = delabelled.map(_.output)

      output.foreach(println)
    }
  }

  case class ParseResult(ops: Seq[GCCCode], env: Env)

  case class ParsedLine(label: Option[String], code: GCCCode)


}

object ExampleLabelledGCC {
  import gcccode._
  implicit def noLabel(gc: GCCCode): LabelledGCC = LabelledGCC(gc, None)
  implicit def addressLiteral(i: Int): Address = AddressLiteral(i)
  implicit def addressLabel(l: String): Address = AddressLabel(l)

  val ex1: Seq[LabelledGCC] = Seq(
    DUM(2),
    LDF("go"),
    LDF("to"),
    LDF("main"),
    RAP(2),
    RTN,
    LDC(1).labelled("main"),
    LD(0, 0),
    AP(1),
    RTN,
    LD(0,0).labelled("to"),
    LDC(1),
    SUB,
    LD(1,0),
    AP(1),
    RTN,
    LD(0,0).labelled("go"),
    LDC(1),
    ADD,
    LD(1,1),
    AP(1),
    RTN
  )
}
