# glexer

[![Package Version](https://img.shields.io/hexpm/v/glexer)](https://hex.pm/packages/glexer)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glexer/)

A lexer for Gleam, written in Gleam!

## Installation

```sh
gleam add glexer
```

Documentation can be found at <https://hexdocs.pm/glexer>.

## Quick Start

```gleam
import glexer

pub fn main() {
  let tokens = "pub fn main() {}"
    |> glexer.new()
    |> glexer.lex()

  // tokens is of type List(#(glexer.Token, glexer.Position))
}
```
