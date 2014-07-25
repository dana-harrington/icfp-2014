package com.nurun.icfp_2014

import com.nurun.icfp_2014.gcccode.{Env, GCCCode}

import scala.io.Source

object Compile {
  def main(args: Array[String]): Unit = {
    args.headOption.foreach { file =>
      val srcText = Source.fromFile(file).getLines()
      // parse
      // compile
      // labelled output
      // delabelled output

      srcText foreach println
    }
  }

  case class ParseResult(ops: Seq[GCCCode], env: Env)

  case class ParsedLine(label: Option[String], code: GCCCode)


}

object ExampleLabelledGCC {
  import gcccode._
  implicit def noLabel(gc: GCCCode): LabelledGCC(gc, None)
  val ex1: Seq[GCCCode] = Seq(
    LabelledGCC(DUM(2)))
}
