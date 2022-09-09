trait CharStream {
  def lookAhead(): Char

  def advance(): Unit
}

trait Output {
  def write(x: Any): Unit
}

class ConsoleSource extends CharStream {
    private var current = System.in.read().toChar
    def advance(): Unit =
        current = System.in.read().toChar
    def lookAhead(): Char = current
}

class ConsoleSink extends Output {
  override def write(x: Any): Unit = println(x)
}
