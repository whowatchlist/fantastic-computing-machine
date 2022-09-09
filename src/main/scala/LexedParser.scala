import Utilities.{Token, UndefinedVariableException, UnexpectedCharacterException, UnexpectedTokenException}

class LexedParser(source: CharStream, sink: Output) {
  private var currentToken = Token.Initial

  private val operators = "+-*/<>:=|&!".toSet
  private val multOps = "*/".toSet
  private val addOps = "+-".toSet
  private val orOps = "|^".toSet
  private val andOps = "&".toSet
  private val relOps = "=#<>".toSet

  private val isMultOp = multOps.contains
  private val isOP = operators.contains
  private val isAddOp = addOps.contains
  private val isOrOp = orOps.contains
  private val isAndOp = andOps.contains
  private val isRelOp = relOps.contains

  val varTable = scala.collection.mutable.Map.empty[String, Int]

  def showState(): String = varTable.map{case (name, value) => s"$name: $value"}.mkString("\n")

  private def expected(cond: Boolean, s: String): Unit =
    if ! cond then throw UnexpectedCharacterException(s + " was expected")

  private def matchChar(c: Char): Unit =
    expected(source.lookAhead() == c, s"\"$c\"")
    source.advance()
    skipWhite()

  private def matchString(t: Token): Unit = expected(currentToken == t, s"\"${t.toString}\"")

  private def skipWhite(): Unit = while source.lookAhead().isSpaceChar do source.advance()

  private def fin(): Unit =
    while source.lookAhead() == '\n' do
      source.advance()
      skipWhite()

  def skipLine(): Unit =
    while source.lookAhead() != '\n' do
      source.advance()
    source.advance()



  def getName: Token =
    var name = ""
    expected(source.lookAhead().isLetter, "Name")
    while source.lookAhead().isLetterOrDigit do
      name += source.lookAhead()
      source.advance()
    skipWhite()
    Token.StrToKeyword(name) match
      case Some(keyword) => keyword
      case None => Token.Name(name)

  def getNum: Token =
    var num = ""
    expected(source.lookAhead().isDigit, "Number")
    while source.lookAhead().isDigit do
      num += source.lookAhead()
      source.advance()
    skipWhite()
    Token.Num(num.toInt)


  def getOP: Token =
    var op = ""
    expected(isOP(source.lookAhead()),"Operator")
    while isOP(source.lookAhead()) do
      op += source.lookAhead()
      source.advance()
    skipWhite()
    Token.Operator(op)

  def scan(): Token =
    val token =
      if source.lookAhead() == '\n' then
        fin()
        Token.Newline
      else if source.lookAhead().isLetter then
        getName
      else if source.lookAhead().isDigit then
        getNum
      else if isOP(source.lookAhead()) then
        getOP
      else
        val ch = source.lookAhead().toString
        source.advance()
        Token.Operator(ch)
    skipWhite()
    token


  def ident(): Int =
    getName match
      case Token.Name(varName) => varTable.get(varName) match
        case Some(value) => value
        case None => throw UndefinedVariableException(s"\"$varName\" is undefined")
      case _ => throw UnexpectedTokenException("Identifier was expected")

  def factor(): Int =
    if source.lookAhead() == '(' then
      matchChar('(')
      val e = expression()
      matchChar(')')
      e
    else if source.lookAhead().isLetter then
      ident()
    else
      getNum match
        case Token.Num(i) => i
        case _ => throw UnexpectedTokenException("Number was expected")

  def signedFactor(): Int =
    source.lookAhead() match
      case '-' =>
        source.advance()
        skipWhite()
        factor() * -1
      case '+' =>
        source.advance()
        skipWhite()
        factor()
      case _ =>
        factor()

  def multiply(left: Int): Int =
    matchChar('*')
    left * factor()

  def divide(left: Int): Int =
    matchChar('/')
    left / factor()

  def secondaryTerm(left: Int): Int =
    var acc = left
    while isMultOp(source.lookAhead()) do
      source.lookAhead() match
        case '*' =>
          acc = multiply(acc)
        case '/' =>
          acc = divide(acc)
    acc

  def term(): Int = secondaryTerm(factor())

  def firstTerm(): Int = secondaryTerm(signedFactor())

  def add(left: Int): Int =
    matchChar('+')
    left + term()

  def subtract(left: Int): Int =
    matchChar('-')
    left - term()

  def expression(): Int =
    var acc = firstTerm()
    while isAddOp(source.lookAhead()) do
      source.lookAhead() match
        case '+' =>
          acc = add(acc)
        case '-' =>
          acc = subtract(acc)
    acc

  def greaterThan(left: Int): Boolean =
    scan() match
      case Token.Operator(">") => left > expression()
      case Token.Operator(">=") => left >= expression()
      case _ => throw UnexpectedTokenException("> or >= was expected")

  def lessThan(left: Int): Boolean =
    scan() match
      case Token.Operator("<") => left > expression()
      case Token.Operator("<=") => left >= expression()
      case _ => throw UnexpectedTokenException("< or <= was expected")

  def equalsR(left: Int): Boolean =
    scan() match
      case Token.Operator("==") => left == expression()
      case _ => throw UnexpectedTokenException("== was expected")

  def notEquals(left: Int): Boolean =
    scan() match
      case Token.Operator("!=") => left != expression()
      case _ => throw UnexpectedTokenException("!= was expected")

  def relation(): Boolean =
    val left = expression()
    source.lookAhead() match
      case '=' => equalsR(left)
      case '!' => notEquals(left)
      case '>' => greaterThan(left)
      case '<' => lessThan(left)
      case _ => throw UnexpectedTokenException("Relation was expected")

  def condition(): Boolean =
    skipWhite()
    matchChar('(')
    val b = relation()
    matchChar(')')
    fin()
    b

  def doIf(): Unit =
    if condition() then
      block()
      seekNext(Set(Token.Endif))
      matchString(Token.Endif)
    else
      seekNext(Set(Token.Else, Token.Endif)) match
        case Token.Endif =>
          matchString(Token.Endif)
          scan()
        case Token.Else =>
          scan()
          block()
          matchString(Token.Endif)
        case _ => throw UnexpectedTokenException("Unclosed control flow")

  def seekNext(tokens: Set[Token]): Token =
    var current = currentToken
    while !tokens.contains(current) do
      skipLine()
      current = scan()
    current

  def assignment(): Unit =
    val varName = currentToken match
      case Token.Name(name) => name
      case _ => throw UnexpectedTokenException("Identifier was expected")
    matchChar('=')
    varTable.update(varName,expression())

  def readValue(): Unit =
    val varToRead = scan() match
      case Token.Name(name) => name
      case _ => throw UnexpectedTokenException("Identifier was expected")
    fin()
    scan() match
      case Token.Num(i) => varTable.update(varToRead, i)
      case _ => throw UnexpectedTokenException("Integer was expected")

  def printValue(): Unit =
    val varToPrint = scan() match
      case Token.Name(name) => name
      case _ => throw UnexpectedTokenException("Identifier was expected")
    varTable.get(varToPrint) match
      case Some(value) => sink.write(value)
      case None => throw UnexpectedTokenException("Undefined variable")

  def block(): Unit =
    currentToken = scan()
    while ! Set(Token.End, Token.Endif, Token.Else).contains(currentToken) do
      currentToken match
        case Token.If => 
          doIf()
          fin()
        case Token.Read =>
          readValue()
          fin()
        case Token.Print =>
          printValue()
          fin()
        case _ => 
          assignment()
          fin()
      currentToken = scan()

  def doProgram(): Unit =
    block()
    matchString(Token.End)
}
