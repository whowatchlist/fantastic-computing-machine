import Utilities.Token
@main def hello(): Unit =
  val lex = LexedParser(ConsoleSource(), ConsoleSink())
  lex.doProgram()
