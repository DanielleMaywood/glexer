import gleam/list
import gleam/string
import glychee/benchmark
import simplifile
import glexer
import glexer_old

pub fn main() {
  let assert Ok(src) = simplifile.read("src/glexer.gleam")
  let src =
    src
    |> list.repeat(10)
    |> string.join("\n")

  benchmark.run(
    [
      benchmark.Function(
        label: "new",
        callable: fn(src) {
          fn() {
            glexer.lex(glexer.new(src))
            Nil
          }
        },
      ),
      benchmark.Function(
        label: "old",
        callable: fn(src) {
          fn() {
            glexer_old.lex(glexer_old.new(src))
            Nil
          }
        },
      ),
    ],
    [benchmark.Data(label: "glexer source", data: src)],
  )
}
