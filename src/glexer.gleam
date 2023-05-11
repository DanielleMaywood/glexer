import glexer/internal/predicates
import glexer/token.{Token}
import gleam/string_builder
import gleam/string
import gleam/list

pub type Position {
  Position(Int)
}

pub opaque type Lexer {
  Lexer(graphemes: List(String), position: Int)
}

pub fn new(source: String) -> Lexer {
  Lexer(graphemes: string.to_graphemes(source), position: 0)
}

pub fn lex(lexer: Lexer) -> List(#(Token, Position)) {
  let #(lexer, #(token, position)) = next(lexer)

  case token {
    token.EndOfFile -> [#(token, position)]
    _ -> [#(token, position), ..lex(lexer)]
  }
}

pub fn next(lexer: Lexer) -> #(Lexer, #(Token, Position)) {
  case lexer.graphemes {
    // End Of File
    [] -> #(lexer, #(token.EndOfFile, Position(lexer.position)))

    // Newline
    ["\r", "\n", ..rest] | ["\n", ..rest] -> {
      // TODO (@DanielleMaywood):
      // There is probably be a more efficient way to do this.
      let line = list.take_while(rest, fn(char) { char != "\n" })
      let is_empty = list.all(line, fn(char) { char == " " || char == "\t" })

      case is_empty {
        True -> #(
          advance(lexer, by: 1 + list.length(line)),
          token(lexer, token.EmptyLine),
        )
        False -> next(advance(lexer, by: 1))
      }
    }

    // Whitespace
    [" ", ..] | ["\t", ..] -> next(advance(lexer, by: 1))

    // Comments
    ["/", "/", "/", "/", ..rest] -> {
      let content = list.take_while(rest, fn(char) { char != "\n" })

      #(
        advance(lexer, by: 4 + list.length(content)),
        token(lexer, token.CommentModule),
      )
    }
    ["/", "/", "/", ..rest] -> {
      let content = list.take_while(rest, fn(char) { char != "\n" })

      let comment =
        content
        |> string_builder.from_strings()
        |> string_builder.to_string()

      #(
        advance(lexer, by: 3 + list.length(content)),
        token(lexer, token.CommentDoc(comment)),
      )
    }
    ["/", "/", ..rest] -> {
      let content = list.take_while(rest, fn(char) { char != "\n" })

      #(
        advance(lexer, by: 2 + list.length(content)),
        token(lexer, token.CommentNormal),
      )
    }

    // Groupings
    ["(", ..] -> #(advance(lexer, by: 1), token(lexer, token.LeftParen))
    [")", ..] -> #(advance(lexer, by: 1), token(lexer, token.RightParen))
    ["{", ..] -> #(advance(lexer, by: 1), token(lexer, token.LeftBrace))
    ["}", ..] -> #(advance(lexer, by: 1), token(lexer, token.RightBrace))
    ["[", ..] -> #(advance(lexer, by: 1), token(lexer, token.LeftSquare))
    ["]", ..] -> #(advance(lexer, by: 1), token(lexer, token.RightSquare))

    // Other Punctuation
    [":", ..] -> #(advance(lexer, by: 1), token(lexer, token.Colon))
    [",", ..] -> #(advance(lexer, by: 1), token(lexer, token.Comma))
    [".", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.DotDot))
    [".", ..] -> #(advance(lexer, by: 1), token(lexer, token.Dot))
    ["#", ..] -> #(advance(lexer, by: 1), token(lexer, token.Hash))
    ["!", "=", ..] -> #(advance(lexer, by: 2), token(lexer, token.NotEqual))
    ["!", ..] -> #(advance(lexer, by: 1), token(lexer, token.Bang))
    ["=", "=", ..] -> #(advance(lexer, by: 2), token(lexer, token.EqualEqual))
    ["=", ..] -> #(advance(lexer, by: 1), token(lexer, token.Equal))
    ["|", ">", ..] -> #(advance(lexer, by: 2), token(lexer, token.Pipe))
    ["|", "|", ..] -> #(advance(lexer, by: 2), token(lexer, token.VBarVBar))
    ["|", ..] -> #(advance(lexer, by: 1), token(lexer, token.VBar))
    ["&", "&", ..] -> #(advance(lexer, by: 2), token(lexer, token.AmperAmper))
    ["<", "<", ..] -> #(advance(lexer, by: 2), token(lexer, token.LessLess))
    [">", ">", ..] -> #(
      advance(lexer, by: 2),
      token(lexer, token.GreaterGreater),
    )
    ["<", "-", ..] -> #(advance(lexer, by: 2), token(lexer, token.LeftArrow))
    ["-", ">", ..] -> #(advance(lexer, by: 2), token(lexer, token.RightArrow))

    // String Operators
    ["<", ">", ..] -> #(advance(lexer, by: 2), token(lexer, token.LessGreater))

    // Float Operators
    ["+", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.PlusDot))
    ["-", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.MinusDot))
    ["*", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.StarDot))
    ["/", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.SlashDot))
    ["<", "=", ".", ..] -> #(
      advance(lexer, by: 3),
      token(lexer, token.LessEqualDot),
    )
    ["<", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.LessDot))
    [">", "=", ".", ..] -> #(
      advance(lexer, by: 3),
      token(lexer, token.GreaterEqualDot),
    )
    [">", ".", ..] -> #(advance(lexer, by: 2), token(lexer, token.GreaterDot))

    // Int Operators
    ["+", ..] -> #(advance(lexer, by: 1), token(lexer, token.Plus))
    ["-", ..] -> #(advance(lexer, by: 1), token(lexer, token.Minus))
    ["*", ..] -> #(advance(lexer, by: 1), token(lexer, token.Star))
    ["/", ..] -> #(advance(lexer, by: 1), token(lexer, token.Slash))
    ["<", "=", ..] -> #(advance(lexer, by: 2), token(lexer, token.LessEqual))
    ["<", ..] -> #(advance(lexer, by: 1), token(lexer, token.Less))
    [">", "=", ..] -> #(advance(lexer, by: 2), token(lexer, token.GreaterEqual))
    [">", ..] -> #(advance(lexer, by: 1), token(lexer, token.Greater))
    ["%", ..] -> #(advance(lexer, by: 1), token(lexer, token.Percent))

    // Strings
    ["\"", ..rest] -> lex_string(rest, "", lexer.position)

    // Keywords & Literals
    //
    // TODO (@DanielleMaywood):
    // Refactor this to be more readable. Currently we cannot call
    // functions in guard statements which means we can either do
    // a case on multiple booleans, or create a heavily nested structure.
    // Unless benchmarks indicate this approach is widely inefficient
    // then this is how it will be until a better approach is suggested.
    [char, ..] ->
      case
        predicates.is_lower(char),
        predicates.is_upper(char),
        predicates.is_digit(char)
      {
        // Lowercase Name
        True, False, False -> {
          let name =
            lexer.graphemes
            |> list.take_while(predicates.is_alphanum)
            |> string_builder.from_strings()
            |> string_builder.to_string()

          let as_token = case name {
            "as" -> token.As
            "assert" -> token.Assert
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
            _ -> token.Name(name)
          }

          #(advance(lexer, by: string.length(name)), token(lexer, as_token))
        }
        // Uppercase Name
        False, True, False -> {
          let name =
            lexer.graphemes
            |> list.take_while(predicates.is_alphanum)
            |> string_builder.from_strings()
            |> string_builder.to_string()

          let as_token = token.UpperName(name)

          #(advance(lexer, by: string.length(name)), token(lexer, as_token))
        }
        // Number
        False, False, True -> {
          let number = list.take_while(lexer.graphemes, predicates.is_digit)

          let number =
            number
            |> string_builder.from_strings()
            |> string_builder.to_string()

          let as_token = token.Int(number)

          #(advance(lexer, by: string.length(number)), token(lexer, as_token))
        }
        _, _, _ -> panic
      }
  }
}

fn advance(lexer: Lexer, by offset: Int) -> Lexer {
  Lexer(
    graphemes: list.drop(lexer.graphemes, up_to: offset),
    position: lexer.position + offset,
  )
}

fn token(lexer: Lexer, token: Token) -> #(Token, Position) {
  #(token, Position(lexer.position))
}

fn lex_string(
  input: List(String),
  content: String,
  start: Int,
) -> #(Lexer, #(Token, Position)) {
  case input {
    // End of input, the string is unterminated
    [] -> {
      // This should use string.byte_size, but that function does not exist yet.
      let lexer = Lexer([], start + string.length(content) + 1)
      #(lexer, #(token.UnterminatedString(content), Position(start)))
    }

    // A double quote, the string is terminated
    ["\"", ..rest] -> {
      // This should use string.byte_size, but that function does not exist yet.
      let lexer = Lexer(rest, start + string.length(content) + 2)
      #(lexer, #(token.String(content), Position(start)))
    }

    // A backslash escapes the following character
    ["\\" as g1, g2, ..rest] -> lex_string(rest, content <> g1 <> g2, start)

    // Any other character is content in the string
    [g, ..rest] -> lex_string(rest, content <> g, start)
  }
}
