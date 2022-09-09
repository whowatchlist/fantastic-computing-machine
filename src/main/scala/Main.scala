import Utilities.Token
@main def startREPL(): Unit =
  val lex = LexedParser(ConsoleSource(), ConsoleSink())
  lex.doProgram()
