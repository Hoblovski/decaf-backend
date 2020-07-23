package backend

object Util {
  def optionToString[A](x: Option[A]): String = x match {
    case None => ""
    case Some(x) => x.toString
  }

  def quotedString(s: String): String = "\"" + s + "\""

  def unquoteString(s: String): String = s.substring(1, s.length - 1)

  def escaped(str: String): String = {
    str // TODO
  }
}