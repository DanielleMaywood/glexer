pub type Token {
  // Literals
  Name(String)
  UpperName(String)
  DiscardName(String)
  Int(String)
  Float(String)
  String(String)
  CommentDoc(String)

  // Keywords
  As
  Assert
  Case
  Const
  External
  Fn
  If
  Import
  Let
  Opaque
  Panic
  Pub
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
  CommentNormal
  CommentModule
  Blank(String)
  EmptyLine

  // Invalid code tokens
  UnterminatedString(String)
  UnexpectedGrapheme(String)
}
