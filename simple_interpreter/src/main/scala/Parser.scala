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

  private def howDidYouGetHere =
    println("Error: Unexpected newline ot I/O error")
    sys.exit(0)

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
          howDidYouGetHere
    total

  private def factor() =
    lookAhead match
      case Some('(') =>
        expect('(')
        val n = expression()
        expect(')')
        n
      case Some(_) =>
        takeNum()
      case _ =>
        howDidYouGetHere

  private def term() =
    var left = factor()
    while (lookAhead.exists(isMultOp(_)))
      lookAhead match
        case Some('*') =>
          expect('*')
          left = left * factor()
        case Some('/') =>
          expect('/')
          left = left / factor()
        case _ =>
          howDidYouGetHere
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
          howDidYouGetHere
    left


  private def init(): Unit = advance()

  def run(): Unit =
    init()
    println(expression())
}
