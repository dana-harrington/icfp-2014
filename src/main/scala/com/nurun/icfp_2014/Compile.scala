package com.nurun.icfp_2014

import java.io.PrintStream

import com.nurun.icfp_2014.gcccode.{CodeGen, GCCCode}
import com.nurun.icfp_2014.ir.IR
import com.nurun.icfp_2014.parser.Parser
import scala.io.Source
import scala.language.implicitConversions

object Compile {

  def main(args: Array[String]): Unit = {
    val outStream =
      if (args.length >= 2)
        new PrintStream(new java.io.File(args(1)))
      else
        Console.out

    args.headOption match {

      case Some(sourceFileName) =>
        val output = compileFile(sourceFileName)
        printOutput(output, outStream)

      case None =>
        Console.err.println("You must provide a source file")
    }

  }

  def compileFile(file: String): Parser.ParseResult[Seq[GCCCode]] = {
    val sourceCode = Source.fromFile(file).mkString
    Parser.parse(sourceCode)
          .map(s => compileAST(s))
  }

  def compileAST(ast: parser.ProgramAST): Seq[GCCCode] = {
    val ir = IR.translate(ast)
    val labelled = CodeGen.codegen(ir)
    GCCCode.delabel(labelled)
  }

  def printOutput(output: Parser.ParseResult[Seq[GCCCode]], outStream: PrintStream): Unit = {
    output match {
      case parser.Parser.Success(outputCode, _) =>
        val outputText = outputCode.map(_.output).mkString("\n")
        outStream.println(outputText)

      case e: parser.Parser.NoSuccess =>
        Console.err.println(e.msg)
    }
  }
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
