package backend
import backend.TACParser._
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters._
import scala.collection.mutable

class TACParse extends TACBaseVisitor[()] {
  /** Parsing vtable */
  val vtabs = mutable.ArrayBuffer.empty[VTable]
  /** Parsing function. */
  val funcs = mutable.ArrayBuffer.empty[Func]
  val instrs = mutable.ArrayBuffer.empty[Instr]

  private def tempNo(s: TerminalNode) = {
    s.getText.drop(2).toInt
  }

  override def visitVtabDef(ctx: VtabDefContext): Unit = {
    val name = ctx.className.getText
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

  override def visitFuncDef(ctx: FuncDefContext): Unit = {
    instrs.clear()
    visitChildren(ctx)
    val label = FuncLabel(ctx.funcLabel.className.getText,
      ctx.funcLabel.funcName.getText)
    funcs += new Func(label, instrs.toSeq)
  }

  override def visitAssign(ctx: AssignContext): Unit = {
    instrs += Assign(tempNo(ctx.Temp(0)), tempNo(ctx.Temp(1)));
  }
}