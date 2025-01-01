import gleam/bool
import gleam/list
import gleam/string
import glexer
import simplifile

fn test_roundtrip(directory) {
  let assert Ok(files) = simplifile.get_files(directory)
  use file <- list.each(files)
  use <- bool.guard(when: !string.ends_with(file, ".gleam"), return: Nil)
  test_roundtrip_file(file)
}

fn test_roundtrip_file(filename) {
  let assert Ok(file) = simplifile.read(filename)
  let roundtripped =
    glexer.new(file)
    |> glexer.lex
    |> glexer.to_source

  use <- bool.guard(when: file == roundtripped, return: Nil)

  // files are not equal - write the roundtripped file, then panic.
  let received = filename <> ".received"
  let assert Ok(_) = simplifile.write(received, roundtripped)
  let msg = filename <> " could not roundtrip, wrote " <> received
  panic as msg
}

pub fn roundtrip_self_test() {
  test_roundtrip("src")
  test_roundtrip("test")
}

pub fn roundtrip_stdlib_test() {
  // duplicate, here to make extra sure that the stdandard lib code works.
  test_roundtrip("build/packages/gleam_stdlib")
}

pub fn roundtrip_dependencies_test() {
  test_roundtrip("build/packages")
}
