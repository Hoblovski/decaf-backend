package backend

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TAC {
  val vtables = Vector.empty[VTable]
  val funcs = Vector.empty[Func]
}

case class VTable(val name: String, val parent: Option[String], val methods: Array[FuncLabel])

class BasicBlock(val instrs: Seq[Instr]) {
  val seqs, term = if (instrs.isInstanceOf[TerminatorInstr]) {
    (instrs.init, Some(instrs.last))
  } else {
    (instrs, None)
  }
}

case class FuncLabel(val className: String, val methodName: String)
object MainFuncLabel {
  private val mainFunc = FuncLabel("", "main")
  def apply(): FuncLabel = mainFunc
  def unapply(arg: FuncLabel): Boolean = (arg == mainFunc)
}

class Func(val label: FuncLabel, instrs: Seq[Instr]) {
  val bbs = analyzeBasicBlocks(instrs)

  def analyzeBasicBlocks(instrs: Seq[Instr]): Seq[BasicBlock] = {
    val bbs = ArrayBuffer.empty[BasicBlock]
    val bb_instrs = ArrayBuffer.empty[Instr]
    for (instr <- instrs) {
      if (instr.isInstanceOf[Mark] && bb_instrs.nonEmpty) {
        // For now assume no redundant marks
        bbs += new BasicBlock(bb_instrs.toSeq)
        bb_instrs.clear()
      }
      bb_instrs += instr
      if (instr.isInstanceOf[TerminatorInstr]) {
        bbs += new BasicBlock(bb_instrs.toSeq)
        bb_instrs.clear()
      }
    }
    bbs.toSeq
  }
}


// Temp
case class Temp(t: Int)
case class Label(l: String)

// Instructions
trait Instr
trait TerminatorInstr extends Instr

object UnaryOp extends Enumeration {
  type UnaryOp = Value
  val NEG, NOT = Value
  private val repr = this.values zip
    Seq("-", "!")
  def from(s: String) = repr.find(_._2 == s).get._1
}
import backend.UnaryOp._

import scala.collection.mutable.ArrayBuffer

object BinaryOp extends Enumeration {
  type BinaryOp = Value
  val ADD, SUB, MUL, DIV, REM, EQ, NE, LT, GT, LE, GE, LAND, LOR = Value
  private val repr = this.values zip
    Seq("+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||")
  def from(s: String) = repr.find(_._2 == s).get._1
}
import backend.BinaryOp._

object CondBranchOp extends Enumeration {
  type CondBranchOp = Value
  val BEQZ, BNEZ = Value
  private val repr = this.values zip
    Seq("== 0", "!= 0")
  def from(s: String) = repr.find(_._2 == s).get._1
}
import backend.CondBranchOp._

case class Assign(dst: Temp, src: Temp) extends Instr
case class LoadVTab(dst: Temp, clazz: String) extends Instr
case class LoadImm(dst: Temp, imm: Int) extends Instr
case class LoadStrLit(dst: Temp, str: String) extends Instr
case class Unary(dst: Temp, op: UnaryOp, src: Temp) extends Instr
case class Binary(dst: Temp, op: BinaryOp, lhs: Temp, rhs: Temp) extends Instr
case class Branch(to: Label) extends TerminatorInstr
case class CondBranch(src: Temp, op: CondBranchOp, to: Label) extends TerminatorInstr
case class Return(src: Option[Temp]) extends TerminatorInstr
case class Parm(src: Temp) extends Instr
case class IndirectCall(dst: Option[Temp], to: Temp) extends Instr
case class DirectCall(dst: Option[Temp], to: Label) extends Instr
case class Load(dst: Temp, base: Temp, offset: Int) extends Instr
case class Store(src: Temp, base: Temp, offset: Int) extends Instr
// Ignore memo
case class Mark(l: Label) extends Instr