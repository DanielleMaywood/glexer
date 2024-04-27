import gleam/bit_array
import gleam/bool
import gleam/iterator.{type Iterator}
import gleam/string
import glexer/internal/predicates
import glexer/token.{type Token}

pub type Position {
  Position(byte_offset: Int)
}

pub opaque type Lexer {
  Lexer(source: String, position: Int, check_for_minus: Bool)
}

pub fn new(source: String) -> Lexer {
  Lexer(source: source, position: 0, check_for_minus: False)
}

pub fn iterator(lexer: Lexer) -> Iterator(#(Token, Position)) {
  use lexer <- iterator.unfold(from: lexer)

  case next(lexer) {
    #(_lexer, #(token.EndOfFile, _position)) -> iterator.Done
    #(lexer, token) -> iterator.Next(element: token, accumulator: lexer)
  }
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  iterator(lexer)
  |> iterator.to_list()
}

fn newline(lexer: Lexer, src: String, size: Int) -> #(Lexer, #(Token, Position)) {
  let start = lexer.position
  case consume_whitespace(Lexer(..lexer, source: src, position: start + size)) {
    #(lexer, True) -> #(lexer, #(token.EmptyLine, Position(start)))
    #(lexer, False) -> next(lexer)
  }
}

fn consume_whitespace(lexer: Lexer) -> #(Lexer, Bool) {
  case lexer.source {
    "" | "\n" <> _ | "\r\n" <> _ -> #(lexer, True)
    " " <> rest ->
      consume_whitespace(
        Lexer(..lexer, source: rest, position: lexer.position + 1),
      )
    "\t" <> rest ->
      consume_whitespace(
        Lexer(..lexer, source: rest, position: lexer.position + 1),
      )
    _ -> #(lexer, False)
  }
}

fn comment(
  lexer: Lexer,
  src: String,
  start: Int,
  size: Int,
  token: Token,
) -> #(Lexer, #(Token, Position)) {
  case src {
    "\n" <> _ | "\r\n" <> _ -> #(
      Lexer(..lexer, source: src, position: start + size),
      #(token, Position(start)),
    )
    _ -> {
      case string.pop_grapheme(src) {
        Error(_) -> #(Lexer(..lexer, source: src, position: start + size), #(
          token,
          Position(start),
        ))
        Ok(#(char, rest)) ->
          comment(lexer, rest, start, size + byte_size(char), token)
      }
    }
  }
}

fn doc_comment(
  lexer: Lexer,
  src: String,
  start: Int,
  size: Int,
  content: String,
) -> #(Lexer, #(Token, Position)) {
  case src {
    "\n" <> _ | "\r\n" <> _ -> #(
      Lexer(..lexer, source: src, position: start + size),
      #(token.CommentDoc(content), Position(start)),
    )
    _ -> {
      case string.pop_grapheme(src) {
        Error(_) -> #(Lexer(..lexer, source: src, position: start + size), #(
          token.CommentDoc(content),
          Position(start),
        ))
        Ok(#(char, rest)) -> {
          let size = size + byte_size(char)
          doc_comment(lexer, rest, start, size, content <> char)
        }
      }
    }
  }
}

fn byte_size(string: String) -> Int {
  bit_array.byte_size(<<string:utf8>>)
}

pub fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  use <- bool.lazy_guard(when: lexer.check_for_minus, return: fn() {
    let lexer = Lexer(..lexer, check_for_minus: False)

    case lexer.source {
      "-" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Minus))
      _ -> next(lexer)
    }
  })

  case lexer.source {
    // Newline
    "\r\n" <> rest -> newline(lexer, rest, 2)
    "\n" <> rest -> newline(lexer, rest, 1)

    // Whitespace
    " " <> rest | "\t" <> rest -> next(advance(lexer, rest, 1))

    // Comments
    "////" <> rest ->
      comment(lexer, rest, lexer.position, 4, token.CommentModule)
    "///" <> rest -> doc_comment(lexer, rest, lexer.position, 3, "")
    "//" <> rest -> comment(lexer, rest, lexer.position, 2, token.CommentNormal)

    // Groupings
    "(" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.LeftParen))
    ")" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.RightParen))
    "{" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.LeftBrace))
    "}" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.RightBrace))
    "[" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.LeftSquare))
    "]" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.RightSquare))

    // Other Punctuation
    "@" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.At))
    ":" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Colon))
    "," <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Comma))
    ".." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.DotDot))
    "." <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Dot))
    "#" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Hash))
    "!=" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.NotEqual))
    "!" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Bang))
    "==" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.EqualEqual))
    "=" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Equal))
    "|>" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.Pipe))
    "||" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.VBarVBar))
    "|" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.VBar))
    "&&" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.AmperAmper))
    "<<" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.LessLess))
    ">>" <> rest -> #(
      advance(lexer, rest, 2),
      token(lexer, token.GreaterGreater),
    )
    "<-" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.LeftArrow))
    "->" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.RightArrow))

    // String Operators
    "<>" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.LessGreater))

    // Float Operators
    "+." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.PlusDot))
    "-." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.MinusDot))
    "*." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.StarDot))
    "/." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.SlashDot))
    "<=." <> rest -> #(
      advance(lexer, rest, 3),
      token(lexer, token.LessEqualDot),
    )
    "<." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.LessDot))
    ">=." <> rest -> #(
      advance(lexer, rest, 3),
      token(lexer, token.GreaterEqualDot),
    )
    ">." <> rest -> #(advance(lexer, rest, 2), token(lexer, token.GreaterDot))

    "0b" <> source -> lex_binary(lexer, source, "0b", lexer.position)
    "0o" <> source -> lex_octal(lexer, source, "0o", lexer.position)
    "0x" <> source -> lex_hexadecimal(lexer, source, "0x", lexer.position)

    "0" <> source -> lex_number(lexer, source, "0", LexInt, lexer.position)
    "1" <> source -> lex_number(lexer, source, "1", LexInt, lexer.position)
    "2" <> source -> lex_number(lexer, source, "2", LexInt, lexer.position)
    "3" <> source -> lex_number(lexer, source, "3", LexInt, lexer.position)
    "4" <> source -> lex_number(lexer, source, "4", LexInt, lexer.position)
    "5" <> source -> lex_number(lexer, source, "5", LexInt, lexer.position)
    "6" <> source -> lex_number(lexer, source, "6", LexInt, lexer.position)
    "7" <> source -> lex_number(lexer, source, "7", LexInt, lexer.position)
    "8" <> source -> lex_number(lexer, source, "8", LexInt, lexer.position)
    "9" <> source -> lex_number(lexer, source, "9", LexInt, lexer.position)
    "-0" <> source -> lex_number(lexer, source, "-0", LexInt, lexer.position)
    "-1" <> source -> lex_number(lexer, source, "-1", LexInt, lexer.position)
    "-2" <> source -> lex_number(lexer, source, "-2", LexInt, lexer.position)
    "-3" <> source -> lex_number(lexer, source, "-3", LexInt, lexer.position)
    "-4" <> source -> lex_number(lexer, source, "-4", LexInt, lexer.position)
    "-5" <> source -> lex_number(lexer, source, "-5", LexInt, lexer.position)
    "-6" <> source -> lex_number(lexer, source, "-6", LexInt, lexer.position)
    "-7" <> source -> lex_number(lexer, source, "-7", LexInt, lexer.position)
    "-8" <> source -> lex_number(lexer, source, "-8", LexInt, lexer.position)
    "-9" <> source -> lex_number(lexer, source, "-9", LexInt, lexer.position)

    // Int Operators
    "+" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Plus))
    "-" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Minus))
    "*" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Star))
    "/" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Slash))
    "<=" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.LessEqual))
    "<" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Less))
    ">=" <> rest -> #(advance(lexer, rest, 2), token(lexer, token.GreaterEqual))
    ">" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Greater))
    "%" <> rest -> #(advance(lexer, rest, 1), token(lexer, token.Percent))

    // Strings
    "\"" <> rest -> lex_string(lexer, rest, "", lexer.position)

    // Discard
    "_" <> rest -> {
      let #(name, rest) = take_content(rest, "", predicates.is_name_grapheme)

      #(
        Lexer(..lexer, source: rest, position: lexer.position + byte_size(name)),
        token(lexer, token.DiscardName(name)),
      )
    }

    // Keywords & Literals
    // Lowercase Name
    "a" <> _
    | "b" <> _
    | "c" <> _
    | "d" <> _
    | "e" <> _
    | "f" <> _
    | "g" <> _
    | "h" <> _
    | "i" <> _
    | "j" <> _
    | "k" <> _
    | "l" <> _
    | "m" <> _
    | "n" <> _
    | "o" <> _
    | "p" <> _
    | "q" <> _
    | "r" <> _
    | "s" <> _
    | "t" <> _
    | "u" <> _
    | "v" <> _
    | "w" <> _
    | "x" <> _
    | "y" <> _
    | "z" <> _ -> {
      let #(name, rest) =
        take_content(lexer.source, "", predicates.is_name_grapheme)
      let as_token = case name {
        "assert" -> token.Assert
        "as" -> token.As
        "case" -> token.Case
        "const" -> token.Const
        "external" -> token.External
        "fn" -> token.Fn
        "if" -> token.If
        "import" -> token.Import
        "let" -> token.Let
        "opaque" -> token.Opaque
        "panic" -> token.Panic
        "pub" -> token.Pub
        "todo" -> token.Todo
        "type" -> token.Type
        "use" -> token.Use
        name -> token.Name(name)
      }

      #(
        Lexer(
          source: rest,
          position: lexer.position + byte_size(name),
          check_for_minus: True,
        ),
        token(lexer, as_token),
      )
    }

    // Uppercase Name
    "A" <> _
    | "B" <> _
    | "C" <> _
    | "D" <> _
    | "E" <> _
    | "F" <> _
    | "G" <> _
    | "H" <> _
    | "I" <> _
    | "J" <> _
    | "K" <> _
    | "L" <> _
    | "M" <> _
    | "N" <> _
    | "O" <> _
    | "P" <> _
    | "Q" <> _
    | "R" <> _
    | "S" <> _
    | "T" <> _
    | "U" <> _
    | "V" <> _
    | "W" <> _
    | "X" <> _
    | "Y" <> _
    | "Z" <> _ -> {
      let #(name, rest) =
        take_content(lexer.source, "", predicates.is_upname_grapheme)
      let as_token = token.UpperName(name)
      #(
        Lexer(..lexer, source: rest, position: lexer.position + byte_size(name)),
        token(lexer, as_token),
      )
    }

    _ -> {
      case string.pop_grapheme(lexer.source) {
        // End Of File
        Error(_) -> #(lexer, #(token.EndOfFile, Position(lexer.position)))
        Ok(#(grapheme, rest)) -> {
          let t = token.UnexpectedGrapheme(grapheme)
          #(advance(lexer, rest, byte_size(grapheme)), token(lexer, t))
        }
      }
    }
  }
}

pub fn take_content(
  source: String,
  content: String,
  predicate: fn(String) -> Bool,
) -> #(String, String) {
  case string.pop_grapheme(source) {
    Error(_) -> #(content, "")
    Ok(#(grapheme, rest)) -> {
      case predicate(grapheme) {
        True -> take_content(rest, content <> grapheme, predicate)
        False -> #(content, source)
      }
    }
  }
}

fn advance(lexer: Lexer, source: String, offset: Int) -> Lexer {
  Lexer(..lexer, source: source, position: lexer.position + offset)
}

fn token(lexer: Lexer, token: Token) -> #(Token, Position) {
  #(token, Position(lexer.position))
}

fn lex_string(
  lexer: Lexer,
  input: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case input {
    // A double quote, the string is terminated
    "\"" <> rest -> {
      let lexer =
        Lexer(..lexer, source: rest, position: start + byte_size(content) + 2)
      #(lexer, #(token.String(content), Position(start)))
    }

    // A backslash escapes the following character
    "\\" <> rest -> {
      case string.pop_grapheme(rest) {
        Error(_) -> lex_string(lexer, rest, content <> "\\", start)
        Ok(#(g, rest)) -> lex_string(lexer, rest, content <> "\\" <> g, start)
      }
    }

    // Any other character is content in the string
    _ -> {
      case string.pop_grapheme(input) {
        Ok(#(g, rest)) -> lex_string(lexer, rest, content <> g, start)

        // End of input, the string is unterminated
        Error(_) -> {
          let lexer =
            Lexer(..lexer, source: "", position: start + byte_size(content) + 1)
          #(lexer, #(token.UnterminatedString(content), Position(start)))
        }
      }
    }
  }
}

pub type NumberLexerMode {
  LexInt
  LexFloat
  LexFloatExponent
}

fn lex_number(
  lexer: Lexer,
  input: String,
  content: String,
  mode: NumberLexerMode,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case input {
    // A dot, the number is a float
    "." <> rest if mode == LexInt ->
      lex_number(lexer, rest, content <> ".", LexFloat, start)

    "e-" <> rest if mode == LexFloat ->
      lex_number(lexer, rest, content <> "e-", LexFloatExponent, start)
    "e" <> rest if mode == LexFloat ->
      lex_number(lexer, rest, content <> "e", LexFloatExponent, start)

    "_" <> source -> lex_number(lexer, source, content <> "_", mode, start)
    "0" <> source -> lex_number(lexer, source, content <> "0", mode, start)
    "1" <> source -> lex_number(lexer, source, content <> "1", mode, start)
    "2" <> source -> lex_number(lexer, source, content <> "2", mode, start)
    "3" <> source -> lex_number(lexer, source, content <> "3", mode, start)
    "4" <> source -> lex_number(lexer, source, content <> "4", mode, start)
    "5" <> source -> lex_number(lexer, source, content <> "5", mode, start)
    "6" <> source -> lex_number(lexer, source, content <> "6", mode, start)
    "7" <> source -> lex_number(lexer, source, content <> "7", mode, start)
    "8" <> source -> lex_number(lexer, source, content <> "8", mode, start)
    "9" <> source -> lex_number(lexer, source, content <> "9", mode, start)

    // Anything else and the number is terminated
    source -> {
      let lexer =
        Lexer(
          source: source,
          position: start + byte_size(content),
          check_for_minus: True,
        )

      let token = case mode {
        LexInt -> token.Int(content)
        LexFloat | LexFloatExponent -> token.Float(content)
      }
      #(lexer, #(token, Position(start)))
    }
  }
}

fn lex_binary(
  lexer: Lexer,
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_binary(lexer, source, content <> "_", start)
    "0" <> source -> lex_binary(lexer, source, content <> "0", start)
    "1" <> source -> lex_binary(lexer, source, content <> "1", start)
    source -> {
      let lexer =
        Lexer(..lexer, source: source, position: start + byte_size(content))
      #(lexer, #(token.Int(content), Position(start)))
    }
  }
}

fn lex_octal(
  lexer: Lexer,
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_octal(lexer, source, content <> "_", start)
    "0" <> source -> lex_octal(lexer, source, content <> "0", start)
    "1" <> source -> lex_octal(lexer, source, content <> "1", start)
    "2" <> source -> lex_octal(lexer, source, content <> "2", start)
    "3" <> source -> lex_octal(lexer, source, content <> "3", start)
    "4" <> source -> lex_octal(lexer, source, content <> "4", start)
    "5" <> source -> lex_octal(lexer, source, content <> "5", start)
    "6" <> source -> lex_octal(lexer, source, content <> "6", start)
    "7" <> source -> lex_octal(lexer, source, content <> "7", start)
    source -> {
      let lexer =
        Lexer(..lexer, source: source, position: start + byte_size(content))
      #(lexer, #(token.Int(content), Position(start)))
    }
  }
}

fn lex_hexadecimal(
  lexer: Lexer,
  source: String,
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case source {
    "_" <> source -> lex_hexadecimal(lexer, source, content <> "_", start)
    "0" <> source -> lex_hexadecimal(lexer, source, content <> "0", start)
    "1" <> source -> lex_hexadecimal(lexer, source, content <> "1", start)
    "2" <> source -> lex_hexadecimal(lexer, source, content <> "2", start)
    "3" <> source -> lex_hexadecimal(lexer, source, content <> "3", start)
    "4" <> source -> lex_hexadecimal(lexer, source, content <> "4", start)
    "5" <> source -> lex_hexadecimal(lexer, source, content <> "5", start)
    "6" <> source -> lex_hexadecimal(lexer, source, content <> "6", start)
    "7" <> source -> lex_hexadecimal(lexer, source, content <> "7", start)
    "8" <> source -> lex_hexadecimal(lexer, source, content <> "8", start)
    "9" <> source -> lex_hexadecimal(lexer, source, content <> "9", start)
    "A" <> source -> lex_hexadecimal(lexer, source, content <> "A", start)
    "B" <> source -> lex_hexadecimal(lexer, source, content <> "B", start)
    "C" <> source -> lex_hexadecimal(lexer, source, content <> "C", start)
    "D" <> source -> lex_hexadecimal(lexer, source, content <> "D", start)
    "E" <> source -> lex_hexadecimal(lexer, source, content <> "E", start)
    "F" <> source -> lex_hexadecimal(lexer, source, content <> "F", start)
    "a" <> source -> lex_hexadecimal(lexer, source, content <> "a", start)
    "b" <> source -> lex_hexadecimal(lexer, source, content <> "b", start)
    "c" <> source -> lex_hexadecimal(lexer, source, content <> "c", start)
    "d" <> source -> lex_hexadecimal(lexer, source, content <> "d", start)
    "e" <> source -> lex_hexadecimal(lexer, source, content <> "e", start)
    "f" <> source -> lex_hexadecimal(lexer, source, content <> "f", start)
    source -> {
      let lexer =
        Lexer(..lexer, source: source, position: start + byte_size(content))
      #(lexer, #(token.Int(content), Position(start)))
    }
  }
}
