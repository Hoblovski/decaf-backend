package backend

class TAC {
  val vtables = Vector.empty[VTable]
  val funcs = Vector.empty[Func]
}

case class VTable(val name: String, val parent: Option[String], val methods: Array[FuncLabel])

class BasicBlock(instrs: Seq[Instr]) {
  val seqs: Seq[Instr] = instrs.init
  val terminator: Instr = instrs.last
}

case class FuncLabel(val className: String, val methodName: String)

class Func(val label: FuncLabel, instrs: Seq[Instr]) {
}

trait Instr

object UnaryOp extends Enumeration {
  type UnaryOp = Value
  val NEG, NOT = Value
}

object BinaryOp extends Enumeration {
  type BinaryOp = Value
  val ADD, SUB, MUL, DIV, REM, EQ, NE, LT, GT, LE, GE, LAND, LOR = Value
}

case class Assign(dst: Int, src: Int) extends Instr