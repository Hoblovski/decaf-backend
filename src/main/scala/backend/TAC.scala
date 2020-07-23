package backend

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TACProg(val vtabs: Seq[VTable], val funcs: Seq[Func])

trait TACProgVisitor {
  def visitProg(prog: TACProg): Unit = {
    prog.vtabs foreach visitVTable
    prog.funcs foreach visitFunc
  }
  def visitVTable(vtab: VTable): Unit = { }
  def visitFunc(func: Func): Unit = {
    func.bbs foreach visitBasicBlock
  }
  def visitBasicBlock(bb: BasicBlock): Unit = {
    bb.instrs foreach visitInstr
  }
  def visitInstr(instr: Instr): Unit = {}
}

case class VTable(val name: String, val parent: Option[String], val methods: Array[FuncLabel]) {
  override def toString: String = {
    val leader = s"VTABLE<${name}>:"
    val parentStr = parent match {
      case None => "NULL"
      case Some(p) => s"VTABLE<${p}>"
    }
    val classStr = Util.quotedString(name)
    val methodStrs = methods map (_.toString)
    (leader +: ((parentStr +: classStr +: methodStrs) map ("    " + _))).mkString("\n")
  }
}

class BasicBlock(val instrs: Seq[Instr]) {
  val seqs, term = if (instrs.isInstanceOf[TerminatorInstr]) {
    (instrs.init, Some(instrs.last))
  } else {
    (instrs, None)
  }
}

case class FuncLabel(val className: String, val methodName: String) {
  override def toString: String = this match {
      case MainFuncLabel() => "main"
      case IntrinsicFuncLabel(s) => methodName
      case FuncLabel(c, f) => s"FUNCTION<${c}.${f}>"
    }
}

object MainFuncLabel {
  private val mainFunc = FuncLabel("", "main")
  def apply(): FuncLabel = mainFunc
  def unapply(arg: FuncLabel): Boolean = (arg == mainFunc)
}

object IntrinsicFuncLabel {
  private val intrinsics = Set("_Alloc", "_ReadLine", "_ReadInteger", "_StringEqual", "_PrintInt", "_PrintString", "_PrintBool", "_Halt")
  private val todo = FuncLabel("", "todo")
  def apply(l: String): FuncLabel =
    if (intrinsics contains l) FuncLabel("", l)
    else throw new IllegalArgumentException(s"Invalid intrinsic ${l}")
  def unapply(arg: FuncLabel): Option[String] =
    if (arg.className.nonEmpty) None
    else if (intrinsics contains arg.methodName) Some(arg.methodName)
    else throw new IllegalArgumentException(s"Invalid FuncLabel: ${arg}")
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

  override def toString: String = {
    val header = s"${label}:"
    val f = (x: Instr) => if (x.isInstanceOf[Mark]) x.toString else ("    " + x.toString)
    val instrs = bbs flatMap  (_.instrs map (f))
    (header +: instrs).mkString("\n")
  }
}


// Temp
case class Temp(t: Int) {
  override def toString: String = s"_T${t}"
}
case class Label(l: String) {
  override def toString: String = s"${l}"
}

// Instructions
trait Instr
trait TerminatorInstr extends Instr

object UnaryOp extends Enumeration {
  type UnaryOp = Value
  val NEG, NOT = Value
  private val repr = this.values zip
    Seq("-", "!")
  def from(s: String) = repr.find(_._2 == s).get._1
  def to(s: Value): String = repr.find(_._1 == s).get._2
}
import backend.UnaryOp._

import scala.collection.mutable.ArrayBuffer

object BinaryOp extends Enumeration {
  type BinaryOp = Value
  val ADD, SUB, MUL, DIV, REM, EQ, NE, LT, GT, LE, GE, LAND, LOR = Value
  private val repr = this.values zip
    Seq("+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", "&&", "||")
  def from(s: String) = repr.find(_._2 == s).get._1
  def to(s: Value): String = repr.find(_._1 == s).get._2
}
import backend.BinaryOp._

object CondBranchOp extends Enumeration {
  type CondBranchOp = Value
  val BEQZ, BNEZ = Value
  private val repr = this.values zip
    Seq("== 0", "!= 0")
  def from(s: String) = repr.find(_._2 == s).get._1
  def to(s: Value): String = repr.find(_._1 == s).get._2
}
import backend.CondBranchOp._

case class Assign(dst: Temp, src: Temp) extends Instr {
  override def toString: String = s"${dst} = ${src}"
}
case class LoadVTab(dst: Temp, clazz: String) extends Instr {
  override def toString: String = s"${dst} = VTABLE<${clazz}>"
}
case class LoadImm(dst: Temp, imm: Int) extends Instr {
  override def toString: String = s"${dst} = ${imm}"
}
case class LoadStrLit(dst: Temp, str: String) extends Instr {
  override def toString: String = s"${dst} = ${Util.quotedString(str)}"
}
case class Unary(dst: Temp, op: UnaryOp, src: Temp) extends Instr {
  override def toString: String = s"${dst} = ${UnaryOp.to(op)} ${src}"
}
case class Binary(dst: Temp, op: BinaryOp, lhs: Temp, rhs: Temp) extends Instr {
  override def toString: String = s"${dst} = (${lhs} ${BinaryOp.to(op)} ${rhs})"
}
case class Branch(to: Label) extends TerminatorInstr {
  override def toString: String = s"branch ${to}"
}
case class CondBranch(src: Temp, op: CondBranchOp, to: Label) extends TerminatorInstr {
  override def toString: String = s"if (${src} ${CondBranchOp.to(op)}) branch ${to}"
}
case class Return(src: Option[Temp]) extends TerminatorInstr {
  override def toString: String = s"return ${Util.optionToString(src)}"
}
case class Parm(src: Temp) extends Instr {
  override def toString: String = s"parm ${src}"
}
case class IndirectCall(dst: Option[Temp], to: Temp) extends Instr {
  override def toString: String = dst match {
    case None => s"call ${to}"
    case Some(dst) => s"${dst} = call ${to}"
  }
}
case class DirectCall(dst: Option[Temp], to: FuncLabel) extends Instr {
  override def toString: String = dst match {
    case None => s"call ${to}"
    case Some(dst) => s"${dst} = call ${to}"
  }
}
case class Load(dst: Temp, base: Temp, offset: Int) extends Instr {
  override def toString: String = s"${dst} = *(${base} ${if (offset >= 0) '+' else '-'} ${offset.abs})"
}
case class Store(src: Temp, base: Temp, offset: Int) extends Instr {
  override def toString: String = s"*(${base} ${if (offset >= 0) '+' else '-'} ${offset.abs}) = ${src}"

}
// Ignore memo
case class Mark(l: Label) extends Instr {
  override def toString: String = s"${l}:"
}