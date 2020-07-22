package backend

import java.io.FileInputStream

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.antlr.v4.runtime.BailErrorStrategy

object Main {
  def main(args: Array[String]): Unit = {
    val istream = args match {
      case Array(inputfile) => new FileInputStream(inputfile)
      case _ => System.in
    }
    val input = CharStreams.fromStream(istream)
    val lexer = new TACLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new TACParser(tokens)

    val visitor = new TACParse()
    visitor.visit(parser.prog())
    println(visitor.vtabs)
  }
}
