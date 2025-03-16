import gleam/int
import gleam/list
import gleam/result
import gleam/string
import glexer/token.{type Token}

pub opaque type Lexer {
  Lexer(
    original_source: String,
    source: String,
    byte_offset: Int,
    preserve_whitespace: Bool,
    preserve_comments: Bool,
    mode: LexerMode,
  )
}

type LexerMode {
  Normal
  CheckForMinus
  CheckForNestedDot
  CheckForNestedDotOrMinus
  HasNestedDot
}

pub type Position {
  Position(byte_offset: Int)
}

pub fn new(source: String) -> Lexer {
  Lexer(
    original_source: source,
    source:,
    byte_offset: 0,
    preserve_whitespace: True,
    preserve_comments: True,
    mode: Normal,
  )
}

pub fn discard_whitespace(lexer: Lexer) -> Lexer {
  Lexer(..lexer, preserve_whitespace: False)
}

pub fn discard_comments(lexer: Lexer) -> Lexer {
  Lexer(..lexer, preserve_comments: False)
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  do_lex(lexer, [])
  |> list.reverse
}

fn do_lex(lexer: Lexer, tokens: List(#(Token, Position))) {
  case next(lexer) {
    #(_lexer, #(token.EndOfFile, _)) -> tokens
    #(lexer, token) -> do_lex(lexer, [token, ..tokens])
  }
}

fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  case lexer.mode {
    CheckForMinus ->
      case check_for_minus(lexer) {
        Ok(#(lexer, token)) -> #(lexer, token)
        Error(Nil) -> next(Lexer(..lexer, mode: Normal))
      }

    CheckForNestedDot ->
      case check_for_nested_dot(lexer) {
        Ok(#(lexer, token)) -> #(lexer, token)
        Error(Nil) -> next(Lexer(..lexer, mode: Normal))
      }

    CheckForNestedDotOrMinus ->
      case check_for_nested_dot(lexer) {
        Ok(#(lexer, token)) -> #(lexer, token)
        Error(Nil) ->
          case check_for_minus(lexer) {
            Ok(#(lexer, token)) -> #(lexer, token)
            Error(Nil) -> next(Lexer(..lexer, mode: Normal))
          }
      }

    HasNestedDot ->
      case lexer.source {
        "0" <> source
        | "1" <> source
        | "2" <> source
        | "3" <> source
        | "4" <> source
        | "5" <> source
        | "6" <> source
        | "7" <> source
        | "8" <> source
        | "9" <> source -> {
          let byte_offset = lexer.byte_offset
          let #(lexer, int) =
            advance(lexer, source, 1)
            |> lex_digits(byte_offset, 1)

          let lexer = Lexer(..lexer, mode: CheckForNestedDot)
          #(lexer, #(token.Int(int), Position(byte_offset:)))
        }

        _ -> next(Lexer(..lexer, mode: Normal))
      }

    Normal ->
      case lexer.source {
        // Whitespace
        " " <> source | "\n" <> source | "\r" <> source | "\t" <> source ->
          advance(lexer, source, 1)
          |> whitespace(lexer.byte_offset, 1)

        // Comments
        "////" <> source ->
          advance(lexer, source, 4)
          |> comment(token.CommentModule, "", lexer.byte_offset)
        "///" <> source ->
          advance(lexer, source, 3)
          |> comment(token.CommentDoc, "", lexer.byte_offset)
        "//" <> source ->
          advance(lexer, source, 2)
          |> comment(token.CommentNormal, "", lexer.byte_offset)

        // Groupings
        "(" <> source -> token(lexer, token.LeftParen, source, 1)
        ")" <> source -> token(lexer, token.RightParen, source, 1)
        "{" <> source -> token(lexer, token.LeftBrace, source, 1)
        "}" <> source -> token(lexer, token.RightBrace, source, 1)
        "[" <> source -> token(lexer, token.LeftSquare, source, 1)
        "]" <> source -> token(lexer, token.RightSquare, source, 1)

        // Other Punctuation
        "@" <> source -> token(lexer, token.At, source, 1)
        ":" <> source -> token(lexer, token.Colon, source, 1)
        "," <> source -> token(lexer, token.Comma, source, 1)
        ".." <> source -> token(lexer, token.DotDot, source, 2)
        "." <> source -> token(lexer, token.Dot, source, 1)
        "#" <> source -> token(lexer, token.Hash, source, 1)
        "!=" <> source -> token(lexer, token.NotEqual, source, 2)
        "!" <> source -> token(lexer, token.Bang, source, 1)
        "==" <> source -> token(lexer, token.EqualEqual, source, 2)
        "=" <> source -> token(lexer, token.Equal, source, 1)
        "|>" <> source -> token(lexer, token.Pipe, source, 2)
        "||" <> source -> token(lexer, token.VBarVBar, source, 2)
        "|" <> source -> token(lexer, token.VBar, source, 1)
        "&&" <> source -> token(lexer, token.AmperAmper, source, 2)
        "<<" <> source -> token(lexer, token.LessLess, source, 2)
        ">>" <> source -> token(lexer, token.GreaterGreater, source, 2)
        "<-" <> source -> token(lexer, token.LeftArrow, source, 2)
        "->" <> source -> token(lexer, token.RightArrow, source, 2)

        // String Operators
        "<>" <> source -> token(lexer, token.LessGreater, source, 2)

        // Float Operators
        "+." <> source -> token(lexer, token.PlusDot, source, 2)
        "-." <> source -> token(lexer, token.MinusDot, source, 2)
        "*." <> source -> token(lexer, token.StarDot, source, 2)
        "/." <> source -> token(lexer, token.SlashDot, source, 2)
        "<=." <> source -> token(lexer, token.LessEqualDot, source, 3)
        "<." <> source -> token(lexer, token.LessDot, source, 2)
        ">=." <> source -> token(lexer, token.GreaterEqualDot, source, 3)
        ">." <> source -> token(lexer, token.GreaterDot, source, 2)

        // Binary/Octal/Hexadecimal
        "0b" <> source ->
          advance(lexer, source, 2)
          |> lex_binary(lexer.byte_offset, 2)
        "0o" <> source ->
          advance(lexer, source, 2)
          |> lex_octal(lexer.byte_offset, 2)
        "0x" <> source ->
          advance(lexer, source, 2)
          |> lex_hexadecimal(lexer.byte_offset, 2)

        // Decimal Numbers
        "0" <> source
        | "1" <> source
        | "2" <> source
        | "3" <> source
        | "4" <> source
        | "5" <> source
        | "6" <> source
        | "7" <> source
        | "8" <> source
        | "9" <> source -> {
          advance(lexer, source, 1)
          |> lex_number(LexInt, lexer.byte_offset, 1)
        }

        "-0" <> source
        | "-1" <> source
        | "-2" <> source
        | "-3" <> source
        | "-4" <> source
        | "-5" <> source
        | "-6" <> source
        | "-7" <> source
        | "-8" <> source
        | "-9" <> source -> {
          advance(lexer, source, 2)
          |> lex_number(LexInt, lexer.byte_offset, 2)
        }

        // Int Operators
        "+" <> source -> token(lexer, token.Plus, source, 1)
        "-" <> source -> token(lexer, token.Minus, source, 1)
        "*" <> source -> token(lexer, token.Star, source, 1)
        "/" <> source -> token(lexer, token.Slash, source, 1)
        "<=" <> source -> token(lexer, token.LessEqual, source, 2)
        "<" <> source -> token(lexer, token.Less, source, 1)
        ">=" <> source -> token(lexer, token.GreaterEqual, source, 2)
        ">" <> source -> token(lexer, token.Greater, source, 1)
        "%" <> source -> token(lexer, token.Percent, source, 1)

        // Strings
        "\"" <> source ->
          advance(lexer, source, 1)
          |> lex_string(lexer.byte_offset, 0)

        // Discard
        "_" <> source -> {
          let byte_offset = lexer.byte_offset
          let #(lexer, name) =
            advance(lexer, source, 1)
            |> lex_lowercase_name(byte_offset + 1, 0)

          #(lexer, #(token.DiscardName(name), Position(byte_offset:)))
        }

        // Keywords & Literals (Lowercase)
        "a" <> source
        | "b" <> source
        | "c" <> source
        | "d" <> source
        | "e" <> source
        | "f" <> source
        | "g" <> source
        | "h" <> source
        | "i" <> source
        | "j" <> source
        | "k" <> source
        | "l" <> source
        | "m" <> source
        | "n" <> source
        | "o" <> source
        | "p" <> source
        | "q" <> source
        | "r" <> source
        | "s" <> source
        | "t" <> source
        | "u" <> source
        | "v" <> source
        | "w" <> source
        | "x" <> source
        | "y" <> source
        | "z" <> source -> {
          let byte_offset = lexer.byte_offset
          let #(lexer, name) =
            advance(lexer, source, 1)
            |> lex_lowercase_name(byte_offset, 1)

          let token = case name {
            "as" -> token.As
            "assert" -> token.Assert
            "auto" -> token.Auto
            "case" -> token.Case
            "const" -> token.Const
            "delegate" -> token.Delegate
            "derive" -> token.Derive
            "echo" -> token.Echo
            "else" -> token.Else
            "fn" -> token.Fn
            "if" -> token.If
            "implement" -> token.Implement
            "import" -> token.Import
            "let" -> token.Let
            "macro" -> token.Macro
            "opaque" -> token.Opaque
            "panic" -> token.Panic
            "pub" -> token.Pub
            "test" -> token.Test
            "todo" -> token.Todo
            "type" -> token.Type
            "use" -> token.Use
            _name -> token.Name(name)
          }

          let lexer = Lexer(..lexer, mode: CheckForNestedDotOrMinus)

          #(lexer, #(token, Position(byte_offset:)))
        }

        // Uppercase Name
        "A" <> source
        | "B" <> source
        | "C" <> source
        | "D" <> source
        | "E" <> source
        | "F" <> source
        | "G" <> source
        | "H" <> source
        | "I" <> source
        | "J" <> source
        | "K" <> source
        | "L" <> source
        | "M" <> source
        | "N" <> source
        | "O" <> source
        | "P" <> source
        | "Q" <> source
        | "R" <> source
        | "S" <> source
        | "T" <> source
        | "U" <> source
        | "V" <> source
        | "W" <> source
        | "X" <> source
        | "Y" <> source
        | "Z" <> source -> {
          let byte_offset = lexer.byte_offset
          let #(lexer, name) =
            advance(lexer, source, 1)
            |> lex_uppercase_name(byte_offset, 1)

          #(lexer, #(token.UpperName(name), Position(byte_offset:)))
        }

        _ ->
          case string.pop_grapheme(lexer.source) {
            // We've hit the end of the file
            Error(_) -> #(lexer, #(token.EndOfFile, Position(lexer.byte_offset)))
            // This grapheme was unexpected
            Ok(#(grapheme, source)) -> {
              token(
                lexer,
                token.UnexpectedGrapheme(grapheme),
                source,
                string.byte_size(grapheme),
              )
            }
          }
      }
  }
}

fn lex_digits(lexer: Lexer, start: Int, slice_size: Int) -> #(Lexer, String) {
  case lexer.source {
    "0" <> source
    | "1" <> source
    | "2" <> source
    | "3" <> source
    | "4" <> source
    | "5" <> source
    | "6" <> source
    | "7" <> source
    | "8" <> source
    | "9" <> source ->
      advance(lexer, source, 1)
      |> lex_digits(start, slice_size + 1)
    _ -> {
      let digits = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, digits)
    }
  }
}

fn lex_lowercase_name(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, String) {
  case lexer.source {
    "a" <> source
    | "b" <> source
    | "c" <> source
    | "d" <> source
    | "e" <> source
    | "f" <> source
    | "g" <> source
    | "h" <> source
    | "i" <> source
    | "j" <> source
    | "k" <> source
    | "l" <> source
    | "m" <> source
    | "n" <> source
    | "o" <> source
    | "p" <> source
    | "q" <> source
    | "r" <> source
    | "s" <> source
    | "t" <> source
    | "u" <> source
    | "v" <> source
    | "w" <> source
    | "x" <> source
    | "y" <> source
    | "z" <> source
    | "0" <> source
    | "1" <> source
    | "2" <> source
    | "3" <> source
    | "4" <> source
    | "5" <> source
    | "6" <> source
    | "7" <> source
    | "8" <> source
    | "9" <> source
    | "_" <> source ->
      advance(lexer, source, 1)
      |> lex_lowercase_name(start, slice_size + 1)
    _ -> {
      let name = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, name)
    }
  }
}

fn lex_uppercase_name(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, String) {
  case lexer.source {
    "a" <> source
    | "b" <> source
    | "c" <> source
    | "d" <> source
    | "e" <> source
    | "f" <> source
    | "g" <> source
    | "h" <> source
    | "i" <> source
    | "j" <> source
    | "k" <> source
    | "l" <> source
    | "m" <> source
    | "n" <> source
    | "o" <> source
    | "p" <> source
    | "q" <> source
    | "r" <> source
    | "s" <> source
    | "t" <> source
    | "u" <> source
    | "v" <> source
    | "w" <> source
    | "x" <> source
    | "y" <> source
    | "z" <> source
    | "A" <> source
    | "B" <> source
    | "C" <> source
    | "D" <> source
    | "E" <> source
    | "F" <> source
    | "G" <> source
    | "H" <> source
    | "I" <> source
    | "J" <> source
    | "K" <> source
    | "L" <> source
    | "M" <> source
    | "N" <> source
    | "O" <> source
    | "P" <> source
    | "Q" <> source
    | "R" <> source
    | "S" <> source
    | "T" <> source
    | "U" <> source
    | "V" <> source
    | "W" <> source
    | "X" <> source
    | "Y" <> source
    | "Z" <> source
    | "0" <> source
    | "1" <> source
    | "2" <> source
    | "3" <> source
    | "4" <> source
    | "5" <> source
    | "6" <> source
    | "7" <> source
    | "8" <> source
    | "9" <> source ->
      advance(lexer, source, 1)
      |> lex_uppercase_name(start, slice_size + 1)
    _ -> {
      let name = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, name)
    }
  }
}

fn check_for_minus(lexer: Lexer) -> Result(#(Lexer, #(Token, Position)), Nil) {
  case lexer.source {
    "-" <> source -> {
      let #(lexer, token) = token(lexer, token.Minus, source, 1)

      Ok(#(Lexer(..lexer, mode: Normal), token))
    }

    _ -> Error(Nil)
  }
}

fn check_for_nested_dot(
  lexer: Lexer,
) -> Result(#(Lexer, #(Token, Position)), Nil) {
  case lexer.source {
    ".." <> source -> Ok(token(lexer, token.DotDot, source, 2))
    "." <> source -> {
      case source {
        "0" <> _
        | "1" <> _
        | "2" <> _
        | "3" <> _
        | "4" <> _
        | "5" <> _
        | "6" <> _
        | "7" <> _
        | "8" <> _
        | "9" <> _ -> {
          let #(lexer, token) = token(lexer, token.Dot, source, 1)

          Ok(#(Lexer(..lexer, mode: HasNestedDot), token))
        }
        _ -> Ok(token(lexer, token.Dot, source, 1))
      }
    }

    _ -> Error(Nil)
  }
}

fn whitespace(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    " " <> source | "\t" <> source | "\n" <> source | "\r" <> source ->
      advance(lexer, source, 1)
      |> whitespace(start, slice_size + 1)

    _ ->
      case lexer.preserve_whitespace {
        False -> next(lexer)
        True -> {
          let content = slice_bytes(lexer.original_source, start, slice_size)
          #(lexer, #(token.Space(content), Position(byte_offset: start)))
        }
      }
  }
}

fn comment(
  lexer: Lexer,
  token: fn(String) -> Token,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "\n" <> _ | "\r\n" <> _ ->
      case lexer.preserve_comments {
        True -> #(lexer, #(token(content), Position(byte_offset: start)))
        False -> next(lexer)
      }

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) ->
          case lexer.preserve_comments {
            True -> #(lexer, #(token(content), Position(byte_offset: start)))
            False -> next(lexer)
          }
        Ok(#(grapheme, source)) -> {
          advance(lexer, source, string.byte_size(grapheme))
          |> comment(token, content <> grapheme, start)
        }
      }
  }
}

fn lex_binary(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source | "0" <> source | "1" <> source ->
      advance(lexer, source, 1)
      |> lex_binary(start, slice_size + 1)

    _ -> {
      let content = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, #(token.Int(content), Position(byte_offset: start)))
    }
  }
}

fn lex_octal(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source
    | "0" <> source
    | "1" <> source
    | "2" <> source
    | "3" <> source
    | "4" <> source
    | "5" <> source
    | "6" <> source
    | "7" <> source ->
      advance(lexer, source, 1)
      |> lex_octal(start, slice_size + 1)

    _ -> {
      let content = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, #(token.Int(content), Position(byte_offset: start)))
    }
  }
}

fn lex_hexadecimal(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "_" <> source
    | "0" <> source
    | "1" <> source
    | "2" <> source
    | "3" <> source
    | "4" <> source
    | "5" <> source
    | "6" <> source
    | "7" <> source
    | "8" <> source
    | "9" <> source
    | "a" <> source
    | "A" <> source
    | "b" <> source
    | "B" <> source
    | "c" <> source
    | "C" <> source
    | "d" <> source
    | "D" <> source
    | "e" <> source
    | "E" <> source
    | "f" <> source
    | "F" <> source ->
      advance(lexer, source, 1)
      |> lex_hexadecimal(start, slice_size + 1)

    _ -> {
      let content = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, #(token.Int(content), Position(byte_offset: start)))
    }
  }
}

type LexNumberMode {
  LexInt
  LexFloat
  LexFloatExponent
}

fn lex_number(
  lexer: Lexer,
  mode: LexNumberMode,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source, mode {
    "_" <> source, _
    | "0" <> source, _
    | "1" <> source, _
    | "2" <> source, _
    | "3" <> source, _
    | "4" <> source, _
    | "5" <> source, _
    | "6" <> source, _
    | "7" <> source, _
    | "8" <> source, _
    | "9" <> source, _
    -> {
      advance(lexer, source, 1)
      |> lex_number(mode, start, slice_size + 1)
    }

    "." <> source, LexInt -> {
      advance(lexer, source, 1)
      |> lex_number(LexFloat, start, slice_size + 1)
    }

    "e-" <> source, LexFloat -> {
      advance(lexer, source, 2)
      |> lex_number(LexFloatExponent, start, slice_size + 2)
    }
    "e" <> source, LexFloat -> {
      advance(lexer, source, 1)
      |> lex_number(LexFloatExponent, start, slice_size + 1)
    }

    _, LexInt -> {
      let lexer = Lexer(..lexer, mode: CheckForMinus)
      let content = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, #(token.Int(content), Position(byte_offset: start)))
    }

    _, LexFloat | _, LexFloatExponent -> {
      let lexer = Lexer(..lexer, mode: CheckForMinus)
      let content = slice_bytes(lexer.original_source, start, slice_size)
      #(lexer, #(token.Float(content), Position(byte_offset: start)))
    }
  }
}

fn lex_string(
  lexer: Lexer,
  start: Int,
  slice_size: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer.source {
    "\"" <> source -> {
      let content = slice_bytes(lexer.original_source, start + 1, slice_size)
      #(token.String(content), Position(byte_offset: start))
      |> advanced(lexer, source, 1)
    }

    "\\" <> source ->
      case string.pop_grapheme(source) {
        Error(_) ->
          advance(lexer, source, 1)
          |> lex_string(start, slice_size + 1)

        Ok(#(grapheme, source)) -> {
          let offset = 1 + string.byte_size(grapheme)
          advance(lexer, source, offset)
          |> lex_string(start, slice_size + offset)
        }
      }

    _ ->
      case string.pop_grapheme(lexer.source) {
        Error(_) -> {
          let content =
            slice_bytes(lexer.original_source, start + 1, slice_size)
          #(lexer, #(
            token.UnterminatedString(content),
            Position(byte_offset: start),
          ))
        }
        Ok(#(grapheme, source)) -> {
          let grapheme_size = string.byte_size(grapheme)
          advance(lexer, source, grapheme_size)
          |> lex_string(start, slice_size + grapheme_size)
        }
      }
  }
}

/// Convert the value of a string token to the string it represents.
///
/// This function can fail if the original string contains invalid escape sequences.
///
/// ```gleam
/// unescape_string("\\\"X\\\" marks the spot")
/// // --> Ok("\"X\" marks the spot")
///
/// unescape_string("\\u{1F600}")
/// // --> Ok("😀")
///
/// unescape_string("\\x")
/// // --> Error(Nil)
/// ```
///
pub fn unescape_string(string: String) -> Result(String, Nil) {
  unescape_loop(string, "")
}

fn unescape_loop(escaped: String, unescaped: String) -> Result(String, Nil) {
  case escaped {
    "\\\"" <> escaped -> unescape_loop(escaped, unescaped <> "\"")
    "\\\\" <> escaped -> unescape_loop(escaped, unescaped <> "\\")
    "\\f" <> escaped -> unescape_loop(escaped, unescaped <> "\f")
    "\\n" <> escaped -> unescape_loop(escaped, unescaped <> "\n")
    "\\r" <> escaped -> unescape_loop(escaped, unescaped <> "\r")
    "\\t" <> escaped -> unescape_loop(escaped, unescaped <> "\t")
    "\\u{" <> escaped -> unescape_codepoint(escaped, unescaped, "")
    "\\" <> _ -> Error(Nil)
    _ ->
      case string.pop_grapheme(escaped) {
        Error(_) -> Ok(unescaped)
        Ok(#(grapheme, escaped)) ->
          unescape_loop(escaped, unescaped <> grapheme)
      }
  }
}

fn unescape_codepoint(
  escaped: String,
  unescaped: String,
  codepoint: String,
) -> Result(String, Nil) {
  case string.pop_grapheme(escaped) {
    Ok(#("}", escaped)) -> {
      use codepoint <- result.try(int.base_parse(codepoint, 16))
      use codepoint <- result.try(string.utf_codepoint(codepoint))
      let codepoint = string.from_utf_codepoints([codepoint])
      unescape_loop(escaped, unescaped <> codepoint)
    }

    Ok(#(c, escaped)) -> unescape_codepoint(escaped, unescaped, codepoint <> c)
    Error(Nil) -> Error(Nil)
  }
}

/// Turn a sequence of tokens back to their Gleam source code representation.
///
pub fn to_source(tokens: List(#(Token, Position))) -> String {
  use source, #(tok, _) <- list.fold(tokens, "")
  source <> token.to_source(tok)
}

// ///////////////// //
// Utility Functions //
// ///////////////// //

fn advance(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(..lexer, source:, byte_offset: lexer.byte_offset + offset)
}

fn advanced(
  token: #(Token, Position),
  lexer: Lexer,
  source: String,
  offset: Int,
) -> #(Lexer, #(Token, Position)) {
  #(advance(lexer, source, offset), token)
}

fn token(lexer: Lexer, token: Token, source: String, offset: Int) {
  #(token, Position(byte_offset: lexer.byte_offset))
  |> advanced(lexer, source, offset)
}

// //////////////////// //
// FFI String functions //
// //////////////////// //

@external(erlang, "binary", "part")
@external(javascript, "./glexer.ffi.mjs", "slice_bytes")
fn slice_bytes(string: String, from byte: Int, sized bytes: Int) -> String
