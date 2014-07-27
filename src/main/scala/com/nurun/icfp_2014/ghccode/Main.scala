package com.nurun.icfp_2014.ghccode

/**
 * Created by steven.skelton on 2014-07-27.
 */
object Main extends App {

  //interrupts
  def int0 = {
    //set to A
  }

  def int1 = {
    //set A = Lambda-x
    //set B = Lambda-y
  }

  def int2 = {
    //set A = Lambda(2)-x
    //set B = Lambda(2)-y
  }

  def int3 = {
    //set A = ghost's index
  }

  def int4 = {
    //set A,B to ghost A's starting x,y
  }

  def int5 = {
    //set A,B to ghost A's current x,y
  }

  def int6 = {
    //set A,B to ghost A's vitality,direction
  }

  def int7 = {
    //get map contents in A for A,B = x,y
  }

  def int8 = {
    //debug
  }

  case class Reg(h: Byte)

  /*
  Register use
  A     Interrupt
  B     Interrupt
  C     Position to Move to
  D     -unused
  E     Return PC
  G     Current Position
  H     Current Position
   */

  def generateString = {

    val setMoveDirectionLine = "##setMoveDirection##"
    val ifNotOkDirectionPlusPlusLine = "##ifNotOkDirectionPlusPlusLine##"
    val checkMoveStatusLine = "##checkMoveStatusLine##"
    val setAndEndLine = "##setAndEndLine##"

    //compile string
    val sb = new StringBuilder
    sb.append(s"MOV e,${ ifNotOkDirectionPlusPlusLine }\n")

    /**
     * set a to direction towards lambda-man
     * returns to e
     */
    def setMoveDirection(l: Int): String = {
      s"""INT 3
INT 5                 ; Get our x-ordinate in A.
MOV g,a
MOV h,b
INT 1                 ;get lambdaman
JGT ${ l + 11 },g,a    ; compare x
JGT ${ l + 9 },h,b    ; compare y
MOV c,3               ; SW, so 3
MOV pc,e
MOV c,1               ; NW, so 1
MOV pc,e
JGT ${ l + 14 },h,b      ; compare y
MOV c,0               ; SE, so 0
MOV pc,e
MOV c,2               ; NE, so 2
MOV pc,e"""
    }

    val setMoveDirectionR = sb.lines.length
    sb.append(setMoveDirection(setMoveDirectionR) + "\n")

    /**
     * reads direction c
     * sets a to cell status (>0 moveable)
     * returns to e
     */
    def checkMoveStatus(l: Int): String = {
      s"""INT 3
INT 5
JEQ ${ l + 7 },c,0    ; up?
JEQ ${ l + 9 },c,1    ; right?
JEQ ${ l + 11 },c,2   ; down?
SUB a,1               ; left
MOV pc,${ l + 12 }
SUB b,1               ; up
MOV pc,${ l + 12 }
ADD a,1               ; right
MOV pc,${ l + 12 }
ADD b,1               ; down
INT 7                 ; a!=0 => ok
MOV pc,e"""
    }

    val checkMoveStatusR = sb.lines.length
    sb.append(checkMoveStatus(checkMoveStatusR) + "\n")

    /**
     * set move in c
     * halt
     */
    def setAndEnd() = {
      s"""MOV a,c
INT 0
HLT"""
    }

    val setAndEndLineR = sb.lines.length
    sb.append(setAndEnd + "\n")

    /**
     * assumes c set to direction
     * returns to e
     */
    def ifNotOkDirectionPlusPlus(l: Int): String = {
      s"""MOV e,${ l + 2 }
MOV pc,${ checkMoveStatusLine }
JGT ${ setAndEndLine },a,0
INC a                ; increment direction
MOV pc,e"""
    }

    var ifNotOkDirectionPlusPlusR = sb.lines.length
    sb.append(ifNotOkDirectionPlusPlus(ifNotOkDirectionPlusPlusR) + "\n")

    val markedString = sb.toString
    markedString
      .replaceAll(setMoveDirectionLine, setMoveDirectionR.toString)
      .replaceAll(setAndEndLine, setAndEndLineR.toString)
      .replaceAll(ifNotOkDirectionPlusPlusLine, ifNotOkDirectionPlusPlusR.toString)
      .replaceAll(checkMoveStatusLine, checkMoveStatusR.toString)
      .lines.zipWithIndex.map { case (s, i) => s + "   ; " + i }.mkString("\n")
  }

  println(generateString)
}
