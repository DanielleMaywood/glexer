import gleam/list
import gleam/string
import glexer/internal/predicates
import glexer/token.{type Token}

pub type Position {
  Position(byte_offset: Int)
}

pub fn lex(source: String) -> List(#(Token, Position)) {
  let lexer =
    Lexer(
      source:,
      byte_offset: 0,
      check_for_minus: False,
      check_for_nested_dot: False,
      has_nested_dot: False,
    )

  do_lex(lexer, [])
  |> list.reverse
}

type Lexer {
  Lexer(
    source: String,
    byte_offset: Int,
    check_for_minus: Bool,
    check_for_nested_dot: Bool,
    has_nested_dot: Bool,
  )
}

fn do_lex(lexer: Lexer, tokens: List(#(Token, Position))) {
  case next(lexer) {
    #(_lexer, #(token.EndOfFile, _)) -> tokens
    #(lexer, token) -> do_lex(lexer, [token, ..tokens])
  }
}

fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  case lexer {
    // Do we need to check for a minus?
    Lexer(source: "-" as c <> source, check_for_minus: True, ..) -> {
      let #(lexer, token) =
        token(lexer, token.Minus)
        |> advanced(lexer, source, string.byte_size(c))

      #(Lexer(..lexer, check_for_minus: False), token)
    }

    // Do we need to check for nested dot?
    Lexer(source: ".." as c <> source, check_for_nested_dot: True, ..) ->
      token(lexer, token.DotDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "." as c <> source, check_for_nested_dot: True, ..) -> {
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
          let token = token(lexer, token.Dot)
          let lexer = advance(lexer, source, string.byte_size(c))
          let lexer =
            Lexer(..lexer, check_for_nested_dot: False, has_nested_dot: True)

          #(lexer, token)
        }
        _ ->
          token(lexer, token.Dot)
          |> advanced(lexer, source, string.byte_size(c))
      }
    }

    // Do we need to handle a nested dot?
    Lexer(source: "0" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "1" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "2" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "3" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "4" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "5" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "6" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "7" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "8" as c <> source, byte_offset:, has_nested_dot: True, ..)
    | Lexer(source: "9" as c <> source, byte_offset:, has_nested_dot: True, ..) -> {
      let #(lexer, int) =
        advance(lexer, source, string.byte_size(c))
        |> take_while(c, predicates.is_digit)

      let lexer =
        Lexer(..lexer, has_nested_dot: False, check_for_nested_dot: True)

      #(lexer, #(token.Int(int), Position(byte_offset:)))
    }

    // We want to turn-off check_for_minus check here
    Lexer(check_for_minus: True, ..) ->
      next(Lexer(..lexer, check_for_minus: False))

    // We want to turn-off check_for_nested_dot check here
    Lexer(check_for_nested_dot: True, ..) ->
      next(Lexer(..lexer, check_for_nested_dot: False))

    // We want to turn-off has_nested_dot check here
    Lexer(has_nested_dot: True, ..) ->
      next(Lexer(..lexer, has_nested_dot: False))

    // Newline
    Lexer(source: "\r\n" as c <> source, byte_offset:, ..)
    | Lexer(source: "\n" as c <> source, byte_offset:, ..) ->
      newline(advance(lexer, source, string.byte_size(c)), byte_offset)

    // Whitespace
    Lexer(source: " " as c <> source, ..)
    | Lexer(source: "\t" as c <> source, ..) ->
      next(advance(lexer, source, string.byte_size(c)))

    // Comments
    Lexer(source: "////" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> comment(token.CommentModule, byte_offset)
    Lexer(source: "///" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> doc_comment("", byte_offset)
    Lexer(source: "//" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> comment(token.CommentNormal, byte_offset)

    // Groupings
    Lexer(source: "(" as c <> source, ..) ->
      token(lexer, token.LeftParen)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ")" as c <> source, ..) ->
      token(lexer, token.RightParen)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "{" as c <> source, ..) ->
      token(lexer, token.LeftBrace)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "}" as c <> source, ..) ->
      token(lexer, token.RightBrace)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "[" as c <> source, ..) ->
      token(lexer, token.LeftSquare)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "]" as c <> source, ..) ->
      token(lexer, token.RightSquare)
      |> advanced(lexer, source, string.byte_size(c))

    // Other Punctuation
    Lexer(source: "@" as c <> source, ..) ->
      token(lexer, token.At)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ":" as c <> source, ..) ->
      token(lexer, token.Colon)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "," as c <> source, ..) ->
      token(lexer, token.Comma)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ".." as c <> source, ..) ->
      token(lexer, token.DotDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "." as c <> source, ..) ->
      token(lexer, token.Dot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "#" as c <> source, ..) ->
      token(lexer, token.Hash)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "!=" as c <> source, ..) ->
      token(lexer, token.NotEqual)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "!" as c <> source, ..) ->
      token(lexer, token.Bang)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "==" as c <> source, ..) ->
      token(lexer, token.EqualEqual)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "=" as c <> source, ..) ->
      token(lexer, token.Equal)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "|>" as c <> source, ..) ->
      token(lexer, token.Pipe)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "||" as c <> source, ..) ->
      token(lexer, token.VBarVBar)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "|" as c <> source, ..) ->
      token(lexer, token.VBar)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "&&" as c <> source, ..) ->
      token(lexer, token.AmperAmper)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<<" as c <> source, ..) ->
      token(lexer, token.LessLess)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ">>" as c <> source, ..) ->
      token(lexer, token.GreaterGreater)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<-" as c <> source, ..) ->
      token(lexer, token.LeftArrow)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "->" as c <> source, ..) ->
      token(lexer, token.RightArrow)
      |> advanced(lexer, source, string.byte_size(c))

    // String Operators
    Lexer(source: "<>" as c <> source, ..) ->
      token(lexer, token.LessGreater)
      |> advanced(lexer, source, string.byte_size(c))

    // Float Operators
    Lexer(source: "+." as c <> source, ..) ->
      token(lexer, token.PlusDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "-." as c <> source, ..) ->
      token(lexer, token.MinusDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "*." as c <> source, ..) ->
      token(lexer, token.StarDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "/." as c <> source, ..) ->
      token(lexer, token.SlashDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<=." as c <> source, ..) ->
      token(lexer, token.LessEqualDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<." as c <> source, ..) ->
      token(lexer, token.LessDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ">=." as c <> source, ..) ->
      token(lexer, token.GreaterEqualDot)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ">." as c <> source, ..) ->
      token(lexer, token.GreaterDot)
      |> advanced(lexer, source, string.byte_size(c))

    // Binary/Octal/Hexadecimal
    Lexer(source: "0b" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_binary(c, byte_offset)
    Lexer(source: "0o" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_octal(c, byte_offset)
    Lexer(source: "0x" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_hexadecimal(c, byte_offset)

    // Decimal Numbers
    Lexer(source: "0" as c <> source, byte_offset:, ..)
    | Lexer(source: "1" as c <> source, byte_offset:, ..)
    | Lexer(source: "2" as c <> source, byte_offset:, ..)
    | Lexer(source: "3" as c <> source, byte_offset:, ..)
    | Lexer(source: "4" as c <> source, byte_offset:, ..)
    | Lexer(source: "5" as c <> source, byte_offset:, ..)
    | Lexer(source: "6" as c <> source, byte_offset:, ..)
    | Lexer(source: "7" as c <> source, byte_offset:, ..)
    | Lexer(source: "8" as c <> source, byte_offset:, ..)
    | Lexer(source: "9" as c <> source, byte_offset:, ..)
    | Lexer(source: "-0" as c <> source, byte_offset:, ..)
    | Lexer(source: "-1" as c <> source, byte_offset:, ..)
    | Lexer(source: "-2" as c <> source, byte_offset:, ..)
    | Lexer(source: "-3" as c <> source, byte_offset:, ..)
    | Lexer(source: "-4" as c <> source, byte_offset:, ..)
    | Lexer(source: "-5" as c <> source, byte_offset:, ..)
    | Lexer(source: "-6" as c <> source, byte_offset:, ..)
    | Lexer(source: "-7" as c <> source, byte_offset:, ..)
    | Lexer(source: "-8" as c <> source, byte_offset:, ..)
    | Lexer(source: "-9" as c <> source, byte_offset:, ..) -> {
      advance(lexer, source, string.byte_size(c))
      |> lex_number(c, LexInt, byte_offset)
    }

    // Int Operators
    Lexer(source: "+" as c <> source, ..) ->
      token(lexer, token.Plus)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "-" as c <> source, ..) ->
      token(lexer, token.Minus)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "*" as c <> source, ..) ->
      token(lexer, token.Star)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "/" as c <> source, ..) ->
      token(lexer, token.Slash)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<=" as c <> source, ..) ->
      token(lexer, token.LessEqual)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "<" as c <> source, ..) ->
      token(lexer, token.Less)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ">=" as c <> source, ..) ->
      token(lexer, token.GreaterEqual)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: ">" as c <> source, ..) ->
      token(lexer, token.Greater)
      |> advanced(lexer, source, string.byte_size(c))
    Lexer(source: "%" as c <> source, ..) ->
      token(lexer, token.Percent)
      |> advanced(lexer, source, string.byte_size(c))

    // Strings
    Lexer(source: "\"" as c <> source, byte_offset:, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_string("", byte_offset)

    // Discard
    Lexer(source: "_" as c <> source, byte_offset:, ..) -> {
      let #(lexer, name) =
        advance(lexer, source, string.byte_size(c))
        |> take_while("", predicates.is_name_grapheme)

      #(lexer, #(token.DiscardName(name), Position(byte_offset:)))
    }

    // Keywords & Literals (Lowercase)
    Lexer(source: "a" as c <> source, byte_offset:, ..)
    | Lexer(source: "b" as c <> source, byte_offset:, ..)
    | Lexer(source: "c" as c <> source, byte_offset:, ..)
    | Lexer(source: "d" as c <> source, byte_offset:, ..)
    | Lexer(source: "e" as c <> source, byte_offset:, ..)
    | Lexer(source: "f" as c <> source, byte_offset:, ..)
    | Lexer(source: "g" as c <> source, byte_offset:, ..)
    | Lexer(source: "h" as c <> source, byte_offset:, ..)
    | Lexer(source: "i" as c <> source, byte_offset:, ..)
    | Lexer(source: "j" as c <> source, byte_offset:, ..)
    | Lexer(source: "k" as c <> source, byte_offset:, ..)
    | Lexer(source: "l" as c <> source, byte_offset:, ..)
    | Lexer(source: "m" as c <> source, byte_offset:, ..)
    | Lexer(source: "n" as c <> source, byte_offset:, ..)
    | Lexer(source: "o" as c <> source, byte_offset:, ..)
    | Lexer(source: "p" as c <> source, byte_offset:, ..)
    | Lexer(source: "q" as c <> source, byte_offset:, ..)
    | Lexer(source: "r" as c <> source, byte_offset:, ..)
    | Lexer(source: "s" as c <> source, byte_offset:, ..)
    | Lexer(source: "t" as c <> source, byte_offset:, ..)
    | Lexer(source: "u" as c <> source, byte_offset:, ..)
    | Lexer(source: "v" as c <> source, byte_offset:, ..)
    | Lexer(source: "w" as c <> source, byte_offset:, ..)
    | Lexer(source: "x" as c <> source, byte_offset:, ..)
    | Lexer(source: "y" as c <> source, byte_offset:, ..)
    | Lexer(source: "z" as c <> source, byte_offset:, ..) -> {
      let #(lexer, name) =
        advance(lexer, source, string.byte_size(c))
        |> take_while(c, predicates.is_name_grapheme)

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

      let lexer =
        Lexer(..lexer, check_for_minus: True, check_for_nested_dot: True)

      #(lexer, #(token, Position(byte_offset:)))
    }

    // Uppercase Name
    Lexer(source: "A" as c <> source, byte_offset:, ..)
    | Lexer(source: "B" as c <> source, byte_offset:, ..)
    | Lexer(source: "C" as c <> source, byte_offset:, ..)
    | Lexer(source: "D" as c <> source, byte_offset:, ..)
    | Lexer(source: "E" as c <> source, byte_offset:, ..)
    | Lexer(source: "F" as c <> source, byte_offset:, ..)
    | Lexer(source: "G" as c <> source, byte_offset:, ..)
    | Lexer(source: "H" as c <> source, byte_offset:, ..)
    | Lexer(source: "I" as c <> source, byte_offset:, ..)
    | Lexer(source: "J" as c <> source, byte_offset:, ..)
    | Lexer(source: "K" as c <> source, byte_offset:, ..)
    | Lexer(source: "L" as c <> source, byte_offset:, ..)
    | Lexer(source: "M" as c <> source, byte_offset:, ..)
    | Lexer(source: "N" as c <> source, byte_offset:, ..)
    | Lexer(source: "O" as c <> source, byte_offset:, ..)
    | Lexer(source: "P" as c <> source, byte_offset:, ..)
    | Lexer(source: "Q" as c <> source, byte_offset:, ..)
    | Lexer(source: "R" as c <> source, byte_offset:, ..)
    | Lexer(source: "S" as c <> source, byte_offset:, ..)
    | Lexer(source: "T" as c <> source, byte_offset:, ..)
    | Lexer(source: "U" as c <> source, byte_offset:, ..)
    | Lexer(source: "V" as c <> source, byte_offset:, ..)
    | Lexer(source: "W" as c <> source, byte_offset:, ..)
    | Lexer(source: "X" as c <> source, byte_offset:, ..)
    | Lexer(source: "Y" as c <> source, byte_offset:, ..)
    | Lexer(source: "Z" as c <> source, byte_offset:, ..) -> {
      let #(lexer, name) =
        advance(lexer, source, string.byte_size(c))
        |> take_while(c, predicates.is_upname_grapheme)

      #(lexer, #(token.UpperName(name), Position(byte_offset:)))
    }

    Lexer(source:, ..) ->
      case string.pop_grapheme(source) {
        // We've hit the end of the file
        Error(_) -> #(lexer, #(token.EndOfFile, Position(lexer.byte_offset)))
        // This grapheme was unexpected
        Ok(#(grapheme, source)) -> {
          token(lexer, token.UnexpectedGrapheme(grapheme))
          |> advanced(lexer, source, string.byte_size(grapheme))
        }
      }
  }
}

fn newline(lexer: Lexer, start: Int) -> #(Lexer, #(Token, Position)) {
  case whitespace(lexer) {
    #(lexer, True) -> #(lexer, #(token.EmptyLine, Position(byte_offset: start)))
    #(lexer, False) -> next(lexer)
  }
}

fn whitespace(lexer: Lexer) -> #(Lexer, Bool) {
  case lexer {
    Lexer(source: "", ..)
    | Lexer(source: "\n" <> _, ..)
    | Lexer(source: "\r\n" <> _, ..) -> #(lexer, True)

    Lexer(source: " " as c <> source, ..)
    | Lexer(source: "\t" as c <> source, ..) ->
      whitespace(advance(lexer, source, string.byte_size(c)))

    _ -> #(lexer, False)
  }
}

fn comment(
  lexer: Lexer,
  token: Token,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "\n" <> _, ..) | Lexer(source: "\r\n" <> _, ..) -> {
      #(lexer, #(token, Position(byte_offset: start)))
    }

    Lexer(source:, ..) ->
      case string.pop_grapheme(source) {
        Error(_) -> #(lexer, #(token, Position(byte_offset: start)))
        Ok(#(grapheme, source)) -> {
          advance(lexer, source, string.byte_size(grapheme))
          |> comment(token, start)
        }
      }
  }
}

fn doc_comment(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "\n" <> _, ..) | Lexer(source: "\r\n" <> _, ..) -> {
      #(lexer, #(token.CommentDoc(content), Position(byte_offset: start)))
    }

    Lexer(source:, ..) ->
      case string.pop_grapheme(source) {
        Error(_) -> #(lexer, #(
          token.CommentDoc(content),
          Position(byte_offset: start),
        ))
        Ok(#(grapheme, source)) ->
          advance(lexer, source, string.byte_size(grapheme))
          |> doc_comment(content <> grapheme, start)
      }
  }
}

fn lex_binary(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "_" as c <> source, ..)
    | Lexer(source: "0" as c <> source, ..)
    | Lexer(source: "1" as c <> source, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_binary(content <> c, start)

    _ -> #(lexer, #(token.Int(content), Position(byte_offset: start)))
  }
}

fn lex_octal(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "_" as c <> source, ..)
    | Lexer(source: "0" as c <> source, ..)
    | Lexer(source: "1" as c <> source, ..)
    | Lexer(source: "2" as c <> source, ..)
    | Lexer(source: "3" as c <> source, ..)
    | Lexer(source: "4" as c <> source, ..)
    | Lexer(source: "5" as c <> source, ..)
    | Lexer(source: "6" as c <> source, ..)
    | Lexer(source: "7" as c <> source, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_octal(content <> c, start)

    _ -> #(lexer, #(token.Int(content), Position(byte_offset: start)))
  }
}

fn lex_hexadecimal(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "_" as c <> source, ..)
    | Lexer(source: "0" as c <> source, ..)
    | Lexer(source: "1" as c <> source, ..)
    | Lexer(source: "2" as c <> source, ..)
    | Lexer(source: "3" as c <> source, ..)
    | Lexer(source: "4" as c <> source, ..)
    | Lexer(source: "5" as c <> source, ..)
    | Lexer(source: "6" as c <> source, ..)
    | Lexer(source: "7" as c <> source, ..)
    | Lexer(source: "8" as c <> source, ..)
    | Lexer(source: "9" as c <> source, ..)
    | Lexer(source: "a" as c <> source, ..)
    | Lexer(source: "A" as c <> source, ..)
    | Lexer(source: "b" as c <> source, ..)
    | Lexer(source: "B" as c <> source, ..)
    | Lexer(source: "c" as c <> source, ..)
    | Lexer(source: "C" as c <> source, ..)
    | Lexer(source: "d" as c <> source, ..)
    | Lexer(source: "D" as c <> source, ..)
    | Lexer(source: "e" as c <> source, ..)
    | Lexer(source: "E" as c <> source, ..)
    | Lexer(source: "f" as c <> source, ..)
    | Lexer(source: "F" as c <> source, ..) ->
      advance(lexer, source, string.byte_size(c))
      |> lex_hexadecimal(content <> c, start)

    _ -> #(lexer, #(token.Int(content), Position(byte_offset: start)))
  }
}

type LexNumberMode {
  LexInt
  LexFloat
  LexFloatExponent
}

fn lex_number(
  lexer: Lexer,
  content: String,
  mode: LexNumberMode,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer, mode {
    Lexer(source: "_" as c <> source, ..), _
    | Lexer(source: "0" as c <> source, ..), _
    | Lexer(source: "1" as c <> source, ..), _
    | Lexer(source: "2" as c <> source, ..), _
    | Lexer(source: "3" as c <> source, ..), _
    | Lexer(source: "4" as c <> source, ..), _
    | Lexer(source: "5" as c <> source, ..), _
    | Lexer(source: "6" as c <> source, ..), _
    | Lexer(source: "7" as c <> source, ..), _
    | Lexer(source: "8" as c <> source, ..), _
    | Lexer(source: "9" as c <> source, ..), _
    -> {
      advance(lexer, source, string.byte_size(c))
      |> lex_number(content <> c, mode, start)
    }

    Lexer(source: "." as c <> source, ..), LexInt -> {
      advance(lexer, source, string.byte_size(c))
      |> lex_number(content <> c, LexFloat, start)
    }

    Lexer(source: "e-" as c <> source, ..), LexFloat
    | Lexer(source: "e" as c <> source, ..), LexFloat
    -> {
      advance(lexer, source, string.byte_size(c))
      |> lex_number(content <> c, LexFloatExponent, start)
    }

    _, LexInt -> {
      let lexer = Lexer(..lexer, check_for_minus: True)
      #(lexer, #(token.Int(content), Position(byte_offset: start)))
    }

    _, LexFloat | _, LexFloatExponent -> {
      let lexer = Lexer(..lexer, check_for_minus: True)
      #(lexer, #(token.Float(content), Position(byte_offset: start)))
    }
  }
}

fn lex_string(
  lexer: Lexer,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case lexer {
    Lexer(source: "\"" as c <> source, ..) -> {
      #(token.String(content), Position(byte_offset: start))
      |> advanced(lexer, source, string.byte_size(c))
    }

    Lexer(source: "\\" as c <> source, ..) ->
      case string.pop_grapheme(source) {
        Error(_) ->
          advance(lexer, source, string.byte_size(c))
          |> lex_string(content <> c, start)
        Ok(#(grapheme, source)) -> {
          let offset = string.byte_size(c) + string.byte_size(grapheme)

          advance(lexer, source, offset)
          |> lex_string(content <> c <> grapheme, start)
        }
      }

    Lexer(source:, ..) ->
      case string.pop_grapheme(source) {
        Error(_) -> #(lexer, #(
          token.UnterminatedString(content),
          Position(byte_offset: start),
        ))
        Ok(#(grapheme, source)) ->
          advance(lexer, source, string.byte_size(grapheme))
          |> lex_string(content <> grapheme, start)
      }
  }
}

// ///////////////// //
// Utility Functions //
// ///////////////// //

fn take_while(
  lexer: Lexer,
  content: String,
  predicate: fn(String) -> Bool,
) -> #(Lexer, String) {
  case string.pop_grapheme(lexer.source) {
    Error(_) -> #(lexer, content)
    Ok(#(grapheme, source)) ->
      case predicate(grapheme) {
        True ->
          advance(lexer, source, string.byte_size(grapheme))
          |> take_while(content <> grapheme, predicate)
        False -> #(lexer, content)
      }
  }
}

fn advance(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(..lexer, source:, byte_offset: lexer.byte_offset + offset)
}

fn token(lexer: Lexer, token: Token) -> #(Token, Position) {
  #(token, Position(byte_offset: lexer.byte_offset))
}

fn advanced(
  token: #(Token, Position),
  lexer: Lexer,
  source: String,
  offset: Int,
) -> #(Lexer, #(Token, Position)) {
  #(advance(lexer, source, offset), token)
}
