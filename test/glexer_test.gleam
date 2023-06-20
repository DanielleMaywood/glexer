import glexer/token
import glexer.{Position}
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

pub fn can_lex_groupings_test() {
  "( ) { } [ ]"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.LeftParen, Position(0)),
    #(token.RightParen, Position(2)),
    #(token.LeftBrace, Position(4)),
    #(token.RightBrace, Position(6)),
    #(token.LeftSquare, Position(8)),
    #(token.RightSquare, Position(10)),
  ])
}

pub fn can_lex_int_operators_test() {
  "+ - * / < > <= >= %"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Plus, Position(0)),
    #(token.Minus, Position(2)),
    #(token.Star, Position(4)),
    #(token.Slash, Position(6)),
    #(token.Less, Position(8)),
    #(token.Greater, Position(10)),
    #(token.LessEqual, Position(12)),
    #(token.GreaterEqual, Position(15)),
    #(token.Percent, Position(18)),
  ])
}

pub fn can_lex_float_operators_test() {
  "+. -. *. /. <. >. <=. >=."
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.PlusDot, Position(0)),
    #(token.MinusDot, Position(3)),
    #(token.StarDot, Position(6)),
    #(token.SlashDot, Position(9)),
    #(token.LessDot, Position(12)),
    #(token.GreaterDot, Position(15)),
    #(token.LessEqualDot, Position(18)),
    #(token.GreaterEqualDot, Position(22)),
  ])
}

pub fn can_lex_string_operators_test() {
  "<>"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.LessGreater, Position(0))])
}

pub fn can_lex_other_punctuation_test() {
  ": , # ! != = == | || && << >> |> . .. -> <-"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Colon, Position(0)),
    #(token.Comma, Position(2)),
    #(token.Hash, Position(4)),
    #(token.Bang, Position(6)),
    #(token.NotEqual, Position(8)),
    #(token.Equal, Position(11)),
    #(token.EqualEqual, Position(13)),
    #(token.VBar, Position(16)),
    #(token.VBarVBar, Position(18)),
    #(token.AmperAmper, Position(21)),
    #(token.LessLess, Position(24)),
    #(token.GreaterGreater, Position(27)),
    #(token.Pipe, Position(30)),
    #(token.Dot, Position(33)),
    #(token.DotDot, Position(35)),
    #(token.RightArrow, Position(38)),
    #(token.LeftArrow, Position(41)),
  ])
}

pub fn can_lex_keywords_test() {
  "as assert case const external fn if import let opaque panic pub todo type use"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.As, Position(0)),
    #(token.Assert, Position(3)),
    #(token.Case, Position(10)),
    #(token.Const, Position(15)),
    #(token.External, Position(21)),
    #(token.Fn, Position(30)),
    #(token.If, Position(33)),
    #(token.Import, Position(36)),
    #(token.Let, Position(43)),
    #(token.Opaque, Position(47)),
    #(token.Panic, Position(54)),
    #(token.Pub, Position(60)),
    #(token.Todo, Position(64)),
    #(token.Type, Position(69)),
    #(token.Use, Position(74)),
  ])
}

pub fn can_lex_empty_lines_test() {
  ".
    
    .
    
    "
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Dot, Position(0)),
    #(token.EmptyLine, Position(1)),
    #(token.Dot, Position(11)),
    #(token.EmptyLine, Position(12)),
    #(token.EmptyLine, Position(17)),
  ])
}

pub fn can_lex_comments_test() {
  "//// This is a module comment
   /// This is a doc comment
   // This is a comment
  "
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.CommentModule, Position(0)),
    #(token.CommentDoc(" This is a doc comment"), Position(33)),
    #(token.CommentNormal, Position(62)),
    #(token.EmptyLine, Position(82)),
  ])
}

pub fn can_lex_example_program_01_test() {
  "
pub fn add(lhs, rhs) {
  lhs + rhs
}

pub fn sub(lhs: Int, rhs: Int) -> Int {
  lhs + rhs
}
"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Pub, Position(1)),
    #(token.Fn, Position(5)),
    #(token.Name("add"), Position(8)),
    #(token.LeftParen, Position(11)),
    #(token.Name("lhs"), Position(12)),
    #(token.Comma, Position(15)),
    #(token.Name("rhs"), Position(17)),
    #(token.RightParen, Position(20)),
    #(token.LeftBrace, Position(22)),
    #(token.Name("lhs"), Position(26)),
    #(token.Plus, Position(30)),
    #(token.Name("rhs"), Position(32)),
    #(token.RightBrace, Position(36)),
    #(token.EmptyLine, Position(37)),
    #(token.Pub, Position(39)),
    #(token.Fn, Position(43)),
    #(token.Name("sub"), Position(46)),
    #(token.LeftParen, Position(49)),
    #(token.Name("lhs"), Position(50)),
    #(token.Colon, Position(53)),
    #(token.UpperName("Int"), Position(55)),
    #(token.Comma, Position(58)),
    #(token.Name("rhs"), Position(60)),
    #(token.Colon, Position(63)),
    #(token.UpperName("Int"), Position(65)),
    #(token.RightParen, Position(68)),
    #(token.RightArrow, Position(70)),
    #(token.UpperName("Int"), Position(73)),
    #(token.LeftBrace, Position(77)),
    #(token.Name("lhs"), Position(81)),
    #(token.Plus, Position(85)),
    #(token.Name("rhs"), Position(87)),
    #(token.RightBrace, Position(91)),
    #(token.EmptyLine, Position(92)),
  ])
}

pub fn name_with_underscores_test() {
  "snake_case"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.Name("snake_case"), Position(0))])
}

pub fn string_empty_test() {
  "\"\""
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.String(""), Position(0))])
}

pub fn string_hello_joe_test() {
  "\"Hello, Joe!\""
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.String("Hello, Joe!"), Position(0))])
}

pub fn string_multiline_test() {
  "\"One\nTwo\nThree\""
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.String("One\nTwo\nThree"), Position(0))])
}

pub fn string_unterminated_test() {
  "\"No closing quote"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.UnterminatedString("No closing quote"), Position(0))])
}

pub fn string_escaped_quote_test() {
  "\" \\\" \""
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.String(" \\\" "), Position(0))])
}

pub fn string_newline_escape_code_test() {
  "\" \\n \""
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.String(" \\n "), Position(0))])
}

pub fn float_test() {
  "1.123 4567.89"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Float("1.123"), Position(0)),
    #(token.Float("4567.89"), Position(6)),
  ])
}

pub fn bad_float_test() {
  "1.123.4567.89"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([
    #(token.Float("1.123"), Position(0)),
    #(token.Dot, Position(5)),
    #(token.Float("4567.89"), Position(6)),
  ])
}

pub fn unexpected_grapheme_test() {
  "£"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.UnexpectedGrapheme("£"), Position(0))])
}

pub fn at_test() {
  "@"
  |> glexer.new()
  |> glexer.lex()
  |> should.equal([#(token.At, Position(0))])
}
