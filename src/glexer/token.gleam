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
