package com.nurun.icfp_2014.gcccode

import java.nio.channels.NonWritableChannelException

import org.specs2.mutable._
import com.nurun.icfp_2014._

/**
 * Created by steven.skelton on 2014-07-25.
 */
class ExamplesSpec extends Specification {

  "Example" should {
    "goto.gcc" in {
      val defs: Seq[Def] = Seq(
        Def("go", Seq("x"), App(Var("to"), Seq(App(Prim(AddOp), Seq(Var("x"), Constant(1)))))),
        Def("to", Seq("x"), App(Var("go"), Seq(App(Prim(SubOp), Seq(Var("x"), Constant(1))))))
      )
      val main = App(Var("go"), Seq(Constant(1)))
      val ex1 = Program(defs, main)
      val result = GCCCode.delabel(ExampleLabelledGCC.ex1).map(_.output)

      val compare = result.zip(exFormat(gotogcc))
      compare.foreach {
        case (actual, expected) => println(actual.padTo(20, ' ') + expected)
      }
      for {
        (actual, expected) <- compare
      } yield {
        actual === expected
      }
    }
  }

  def exFormat(s: String) = s.lines.flatMap(l => {
    if (!l.startsWith(" ")) None
    else Some(l.takeWhile(_ != ';').replaceAllLiterally("  ", " ").trim)
  }).toSeq

  val localgcc = """  LDC  21
                   |  LDF  body     ; load body
                   |  AP   1        ; call body with 1 variable in a new frame
                   |  RTN
                   |body:
                   |  LD   0 0      ; var x
                   |  LD   0 0      ; var x
                   |  ADD
                   |  RTN"""

  val gotogcc = """  DUM  2        ; 2 top-level declarations
                  |  LDF  go       ; declare function go
                  |  LDF  to       ; declare function to
                  |  LDF  main     ; main function
                  |  RAP  2        ; load declarations into environment and run main
                  |  RTN           ; final return
                  |main:
                  |  LDC  1
                  |  LD   0 0      ; var go
                  |  AP   1        ; call go(1)
                  |  RTN
                  |to:
                  |  LD   0 0      ; var n
                  |  LDC  1
                  |  SUB
                  |  LD   1 0      ; var go
                  |  AP   1        ; call go(n-1)
                  |  RTN
                  |go:
                  |  LD   0 0      ; var n
                  |  LDC  1
                  |  ADD
                  |  LD   1 1      ; var to
                  |  AP   1        ; call to(n+1)
                  |  RTN""".stripMargin

  val alwaysdown = """  DUM  2        ; 2 top-level declarations
                     |  LDC  2        ; declare constant down
                     |  LDF  step     ; declare function step
                     |  LDF  init     ; init function
                     |  RAP  2        ; load declarations into environment and run init
                     |  RTN           ; final return
                     |init:
                     |  LDC  42
                     |  LD   0 1      ; var step
                     |  CONS
                     |  RTN           ; return (42, step)
                     |step:
                     |  LD   0 0      ; var s
                     |  LDC  1
                     |  ADD
                     |  LD   1 0      ; var down
                     |  CONS
                     |  RTN           ; return (s+1, down)""".stripMargin
}
