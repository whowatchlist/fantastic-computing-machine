import munit.FunSuite
import Utilities.Token

class LexedParserTest extends FunSuite {
  class StringSource(initial: String) extends CharStream {
    private var remaining = initial

    override def lookAhead(): Char = remaining.head

    override def advance(): Unit =
      remaining = remaining.tail
  }

  def linesToString(lines: String *): String = {
    lines.mkString("\n") ++ "\n"
  }

  test("getName") {
    val lex = LexedParser(StringSource("name1\n"), ConsoleSink())
    assertEquals(lex.getName, Token.Name("name1"))
  }

  test("getNumber") {
    val lex = LexedParser(StringSource("152\n"), ConsoleSink())
    assertEquals(lex.getNum, Token.Num(152))
  }

  test("assignmentTest") {
    val lex = LexedParser(StringSource("x = 10\nend\n"), ConsoleSink())
    lex.doProgram()
    assertEquals(lex.varTable("x"), 10)
  }

  test("equalityTest") {
    val lex = LexedParser(StringSource("if (1 == 1)\nn = 5\nendif\nend\n"), ConsoleSink())
    lex.doProgram()
    assertEquals(lex.varTable("n"), 5)
  }

  test("else testing") {
    val input = linesToString(
      "if (99 > 100)",
      "n = 5",
      "else",
      "n = 10",
      "endif",
      "end"
    )

    val lex = LexedParser(StringSource(input), ConsoleSink())
    lex.doProgram()
    assertEquals(lex.varTable("n"), 10)
  }
}
