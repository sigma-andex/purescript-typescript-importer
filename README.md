# purescript-typescript-importer

Experiment of writing a typescript importer for purescript. Very early stage and ongoing development, don't use it yet!

The current state of typescript support can be seen in the testfiles folder. In [`testfiles/original`](./testfiles/original/) are the typescript input files and in [`testfiles/golden`](./testfiles/golden/) the expected output of the typescript support that is currently passing.

## Goals & non-goals

The goal of this project is to create a tool that generates useful and working purescript ffis from typescript type definitions. It is not meant to create a fully-fledged typescript to purescript transpiler, as I don't see much added value to this and it's also a whole different story scope-wise.
It is also (currently) not a goal to go the other way around, i.e. from purescript to typescript.

## Typescript <≡> Purescript type correspondence

See [Typescript <≡> Purescript type correspondence](docs/type-correspondence.md).

## Usage

```bash
# WIP, run it using spago, pass arguments using --exec-args
spago run --exec-args "import <NODE_MODULE>"
```

## Maintainers

- [sigma-andex](https://github.com/sigma-andex/)
