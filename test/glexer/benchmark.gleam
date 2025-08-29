import gleam/io
import gleam/string
import gleamy/bench
import glexer
import simplifile

pub fn main() {
  let assert Ok(source) =
    simplifile.read("./build/packages/gleam_stdlib/src/gleam/list.gleam")

  let source_x1 = source |> string.repeat(1)
  let source_x10 = source |> string.repeat(10)
  let source_x100 = source |> string.repeat(100)

  bench.run(
    [
      bench.Input("src x1", source_x1),
      bench.Input("src x10", source_x10),
      bench.Input("src x100", source_x100),
    ],
    [bench.Function("lex", fn(source) { glexer.new(source) |> glexer.lex })],
    [bench.Duration(5000), bench.Warmup(100)],
  )
  |> bench.table([bench.IPS, bench.Min, bench.P(99)])
  |> io.println
}
