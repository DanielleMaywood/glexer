pub type Token {
  // Literals
  Name(String)
  UpperName(String)
  DiscardName(String)
  Int(String)
  Float(String)
  String(String)
  CommentDoc(String)
  CommentNormal(String)
  CommentModule(String)

  // Keywords
  As
  Assert
  Auto
  Case
  Const
  Delegate
  Derive
  Echo
  Else
  Fn
  If
  Implement
  Import
  Let
  Macro
  Opaque
  Panic
  Pub
  Test
  Todo
  Type
  Use

  // Groupings
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  LeftSquare
  RightSquare

  // Int Operators
  Plus
  Minus
  Star
  Slash
  Less
  Greater
  LessEqual
  GreaterEqual
  Percent

  // Float Operators
  PlusDot
  MinusDot
  StarDot
  SlashDot
  LessDot
  GreaterDot
  LessEqualDot
  GreaterEqualDot

  // String Operators
  LessGreater

  // Other Punctuation
  At
  Colon
  Comma
  Hash
  Bang
  Equal
  EqualEqual
  NotEqual
  VBar
  VBarVBar
  AmperAmper
  LessLess
  GreaterGreater
  Pipe
  Dot
  DotDot
  LeftArrow
  RightArrow
  EndOfFile

  // Extra
  Space(String)

  // Invalid code tokens
  UnterminatedString(String)
  UnexpectedGrapheme(String)
}

/// Turn a token back into its Gleam source representation.
///
pub fn to_source(tok: Token) -> String {
  case tok {
    // Literals
    Name(str) -> str
    UpperName(str) -> str
    Int(str) -> str
    Float(str) -> str
    DiscardName(str) -> "_" <> str
    String(str) -> "\"" <> str <> "\""
    CommentDoc(str) -> "///" <> str
    CommentNormal(str) -> "//" <> str
    CommentModule(str) -> "////" <> str

    // Keywords
    As -> "as"
    Assert -> "assert"
    Auto -> "auto"
    Case -> "case"
    Const -> "const"
    Delegate -> "delegate"
    Derive -> "derive"
    Echo -> "echo"
    Else -> "else"
    Fn -> "fn"
    If -> "if"
    Implement -> "implement"
    Import -> "import"
    Let -> "let"
    Macro -> "macro"
    Opaque -> "opaque"
    Panic -> "panic"
    Pub -> "pub"
    Test -> "test"
    Todo -> "todo"
    Type -> "type"
    Use -> "use"

    // Groupings
    LeftParen -> "("
    RightParen -> ")"
    LeftBrace -> "{"
    RightBrace -> "}"
    LeftSquare -> "["
    RightSquare -> "]"

    // Int Operators
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    Less -> "<"
    Greater -> ">"
    LessEqual -> "<="
    GreaterEqual -> ">="
    Percent -> "%"

    // Float Operators
    PlusDot -> "+."
    MinusDot -> "-."
    StarDot -> "*."
    SlashDot -> "/."
    LessDot -> "<."
    GreaterDot -> ">."
    LessEqualDot -> "<=."
    GreaterEqualDot -> ">=."

    // String Operators
    LessGreater -> "<>"

    // Other Punctuation
    At -> "@"
    Colon -> ":"
    Comma -> ","
    Hash -> "#"
    Bang -> "!"
    Equal -> "="
    EqualEqual -> "=="
    NotEqual -> "!="
    VBar -> "|"
    VBarVBar -> "||"
    AmperAmper -> "&&"
    LessLess -> "<<"
    GreaterGreater -> ">>"
    Pipe -> "|>"
    Dot -> "."
    DotDot -> ".."
    LeftArrow -> "<-"
    RightArrow -> "->"
    EndOfFile -> ""

    // Extra
    Space(str) -> str

    // Invalid code tokens
    UnterminatedString(str) -> "\"" <> str
    UnexpectedGrapheme(str) -> str
  }
}
