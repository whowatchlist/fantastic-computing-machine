object Utilities {

  object Token {
    private val keywordMap = Map(
      "if" -> Token.If,
      "else" -> Token.Else,
      "elseif" -> Token.Elseif,
      "quit" -> Token.Quit,
      "end" -> Token.End,
      "while" -> Token.While,
      "true" -> Token.Bool(true),
      "false" -> Token.Bool(false),
      "read" -> Token.Read,
      "print" -> Token.Print
    )
    val StrToKeyword: String => Option[Token] = keywordMap.get
  }

  enum Token:
    case Initial
    case If
    case Else
    case Elseif
    case End
    case While
    case Quit
    case Newline
    case Read
    case Print
    case Bool(tok: Boolean)
    case Name(tok: String)
    case Num(tok: Int)
    case Operator(tok: String)


  class UnexpectedCharacterException(message: String) extends Exception(message) {

    def this(message: String, cause: Throwable) =
      this(message)
      initCause(cause)

    def this(cause: Throwable) =
      this(Option(cause).map(_.toString).orNull, cause)

    def this() =
      this(null: String)

  }

  class UnexpectedTokenException(message: String) extends Exception(message) {

    def this(message: String, cause: Throwable) =
      this(message)
      initCause(cause)

    def this(cause: Throwable) =
      this(Option(cause).map(_.toString).orNull, cause)

    def this() =
      this(null: String)

  }

  class UndefinedVariableException(message: String) extends Exception(message) {

    def this(message: String, cause: Throwable) =
      this(message)
      initCause(cause)

    def this(cause: Throwable) =
      this(Option(cause).map(_.toString).orNull, cause)

    def this() =
      this(null: String)

  }

}
