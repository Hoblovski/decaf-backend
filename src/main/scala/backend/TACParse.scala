package backend
import java.net.NoRouteToHostException

import backend.{TACParser => P}
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters._
import scala.collection.mutable

class TACParse extends TACBaseVisitor[()] {
  /** Parsing vtable */
  val vtabs = mutable.ArrayBuffer.empty[VTable]
  /** Parsing function. */
  val funcs = mutable.ArrayBuffer.empty[Func]
  val instrs = mutable.ArrayBuffer.empty[Instr]

  private def mkTemp(s: TerminalNode): Temp = {
    Temp( s.getText.drop(2).toInt)
  }

  private def mkMemOperand(ctx: P.MemOperandContext): (Temp, Int) = {
    (mkTemp(ctx.Temp), (if (ctx.Op.getText == "+") 1 else -1) * ctx.Number.getText.toInt)
  }

  private def mkFuncLabel(ctx: P.FuncLabelContext): FuncLabel = {
    if (ctx.className != null)
      FuncLabel(ctx.className.getText, ctx.funcName.getText)
    else ctx.getText match {
      case "main" => MainFuncLabel()
      case s => IntrinsicFuncLabel(s)
    }
  }

  def getTACProg(): TACProg = new TACProg(vtabs.toSeq, funcs.toSeq)

  override def visitVtabDef(ctx: P.VtabDefContext): Unit = {
    val name = Util.unquoteString(ctx.className.getText)
    val parentName = {
      val t = ctx.parentLabel.vtabLabel
      if (t == null) None else Some(t.className.getText)
    }
    val labels = {
      for (l <- ctx.funcLabel.asScala)
        yield FuncLabel(l.className.getText, l.funcName.getText)
    }
    vtabs += VTable(name, parentName, labels.toArray)
  }

  override def visitFuncDef(ctx: P.FuncDefContext): Unit = {
    instrs.clear()
    visitChildren(ctx)
    val f = ctx.funcLabel()
    val label = if (f.className == null) MainFuncLabel() else
      FuncLabel(f.className.getText, f.funcName.getText)
    funcs += new Func(label, instrs.toSeq)
  }

  override def visitAssign(ctx: P.AssignContext): Unit = {
    instrs += Assign(mkTemp(ctx.Temp(0)), mkTemp(ctx.Temp(1)))
  }

  override def visitLoadVTbl(ctx: TACParser.LoadVTblContext): Unit = {
    instrs += LoadVTab(mkTemp(ctx.Temp()), ctx.vtabLabel.className.getText)
  }

  override def visitLoadImm(ctx: TACParser.LoadImmContext): Unit = {
    instrs += LoadImm(mkTemp(ctx.Temp()), ctx.Number.getText.toInt)
  }

  override def visitLoadStrLit(ctx: TACParser.LoadStrLitContext): Unit = {
    instrs += LoadStrLit(mkTemp(ctx.Temp()), Util.escaped(Util.unquoteString(ctx.StrLit.getText)))
  }

  override def visitUnary(ctx: TACParser.UnaryContext): Unit = {
    instrs += Unary(mkTemp(ctx.Temp(0)), UnaryOp.from(ctx.unaryOp.getText), mkTemp(ctx.Temp(1)))
  }

  override def visitBinary(ctx: TACParser.BinaryContext): Unit = {
    instrs += Binary(mkTemp(ctx.Temp(0)), BinaryOp.from(ctx.binaryOp.getText), mkTemp(ctx.Temp(1)), mkTemp(ctx.Temp(2)))
  }

  override def visitBranch(ctx: TACParser.BranchContext): Unit = {
    instrs += Branch(Label(ctx.label.getText))
  }

  override def visitCondBranch(ctx: TACParser.CondBranchContext): Unit = {
    instrs += CondBranch(mkTemp(ctx.Temp()), CondBranchOp.from(ctx.condBrOp.getText), Label(ctx.label.getText))
  }

  override def visitReturn(ctx: TACParser.ReturnContext): Unit = {
    val dst = if (ctx.Temp == null) None else Some(mkTemp(ctx.Temp))
    instrs += Return(dst)
  }

  override def visitParm(ctx: TACParser.ParmContext): Unit = {
    instrs += Parm(mkTemp(ctx.Temp))
  }

  override def visitIndirectCall(ctx: TACParser.IndirectCallContext): Unit = {
    val (dst, src) = if (ctx.Temp(1) == null)
      (None, mkTemp(ctx.Temp(0)))
    else
      (Some(mkTemp(ctx.Temp(0))), mkTemp(ctx.Temp(1)))
    instrs += IndirectCall(dst, src)
  }

  override def visitDirectCall(ctx: TACParser.DirectCallContext): Unit = {
    val dst = if (ctx.Temp() == null) None else Some(mkTemp(ctx.Temp()))
    instrs += DirectCall(dst, mkFuncLabel(ctx.funcLabel()))
  }

  override def visitLoad(ctx: TACParser.LoadContext): Unit = {
    val memOp = mkMemOperand(ctx.memOperand())
    instrs += Load(mkTemp(ctx.Temp()), memOp._1, memOp._2)
  }

  override def visitStore(ctx: TACParser.StoreContext): Unit = {
    val memOp = mkMemOperand(ctx.memOperand())
    instrs += Store(mkTemp(ctx.Temp()), memOp._1, memOp._2);
  }

  override def visitMark(ctx: TACParser.MarkContext): Unit = {
    instrs += Mark(Label(ctx.label().getText))
  }
}