import scala.io.StdIn
import scala.util.Properties.lineSeparator

class Parser {
  private val eol = lineSeparator
  private var lookAhead: Option[Char] = None
  private val inStream = System.in

  private def advance(): Unit =
    lookAhead = inStream.read() match
      case -1 | 10 | 13 => None
      case n => Some(n.toChar)


  private def expected(s: String) =
    println(s"${eol}Error: $s was expected")
    sys.exit(1)

  private def expect(c: Char): Unit =
    if lookAhead.contains(c)
    then advance()
    else expected(c.toString)

  private def takeNum() =
    var total = lookAhead match
      case Some(c) if c.isDigit =>
        advance()
        c.asDigit
      case _ =>
        expected("Integer")
    while lookAhead.exists(_.isDigit)
      do
      lookAhead match
        case Some(c) =>
          total = 10 * total + c.asDigit
          advance()
        case _ =>
          expected("digit")
    total

  private def term() =
    var left = takeNum()
    while (lookAhead.exists(isMultOp(_)))
      lookAhead match
        case Some('*') =>
          expect('*')
          left = left * takeNum()
        case Some('/') =>
          expect('/')
          left = left / takeNum()
        case _ =>
          expected("operator")
    left

  private val isAddOp = Set('+', '-').contains

  private val isMultOp = Set('*', '/').contains

  private def expression(): Int =
    var left =
      lookAhead match
        case Some(c) if isAddOp(c) =>
          0
        case _ =>
          term()
    while lookAhead.exists(isAddOp)
      do
      lookAhead match
        case Some('+') =>
          expect('+')
          left += term()
        case Some('-') =>
          expect('-')
          left -= term()
        case _ =>
          expected("operator")
    left


  private def init(): Unit = advance()

  def run(): Unit =
    init()
    println(expression())
}
