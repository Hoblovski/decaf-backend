package backend

class TACPrint extends TACProgVisitor {
  def emit(x: Any) = println(x)
  override def visitVTable(vtab: VTable): Unit = {
    emit(s"${vtab}\n")
  }
  override def visitFunc(func: Func): Unit = {
    emit(s"\n${func}")
  }
}
