package com.nurun.icfp_2014

import java.io.PrintStream

import com.nurun.icfp_2014.gcccode.{CodeGen, Env, GCCCode}
import com.nurun.icfp_2014.ir.{IR, Example}
import scala.io.Source
import scala.language.implicitConversions

object Compile {
  def main(args: Array[String]): Unit = {
    val sourceFile = args.headOption

    val outStream =
      if (args.length >= 2)
        new PrintStream(new java.io.File(args(1)))
      else
        Console.out

    val output = sourceFile.map { file =>
      //GCCCode.delabel(ExampleLabelledGCC.ex1).map(_.output).foreach(println)
      val sourceCode = Source.fromFile(file).mkString
      val output = parser.Parser.parse(sourceCode).map{ ast =>
        val ir = IR.translate(ast)
        val labelled = CodeGen.codegen(ir)
        val delabelled = GCCCode.delabel(labelled)
        val output = delabelled.map(_.output)

        output.mkString("\n")

      }
      output match {
        case parser.Parser.Success(outputText, _) =>
          outStream.print(outputText)

        case e: parser.Parser.NoSuccess =>
          Console.err.println(e.msg)
      }
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
