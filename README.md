
# jcc

A small C compiler front-end and code generator that reads a C source file and emits assembly. The project includes a test suite with a shell harness and a lexer dump tool for debugging.

## Build

```sh
make
```

This produces a `jcc` executable in the project root.

## Usage

```sh
./jcc input.c output.s
```

Common flags:

- `--dump-tokens` prints lexer tokens to stderr and exits.
- `-v` or `--verbose` prints extra parse/AST debug output to stderr.

Example:

```sh
./jcc --dump-tokens test/test_global.c /tmp/ignored.s
./jcc -v test/test_global.c /tmp/test_global.s
```

## Tests

```sh
make test
```

The test runner is `test/test.sh` and expects `jcc` to be built first.

## Tools

Build the lexer dump utility:

```sh
make lexdump
```

## Clean

```sh
make clean
```

This removes build outputs and test artifacts.
