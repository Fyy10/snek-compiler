# snek-compiler

- [snek-compiler](#snek-compiler)
  - [The Snek Language](#the-snek-language)
    - [Concrete Syntax](#concrete-syntax)
    - [Abstract Syntax (in Rust)](#abstract-syntax-in-rust)
    - [Data Representations](#data-representations)
      - [Tuple structure in heap](#tuple-structure-in-heap)
  - [Usage](#usage)
    - [Compile to assembly](#compile-to-assembly)
    - [Compile to executable binary](#compile-to-executable-binary)
    - [Run the executable](#run-the-executable)
  - [Testing](#testing)

An x86_64 compiler for snek language.

## The Snek Language

### Concrete Syntax

```plain
<prog> := <defn>* <expr>
<defn> := (fun (<name> <name>*) <expr>)
<expr> :=
    | <number>
    | true
    | false
    | input
    | <identifier>
    | (let (<binding>+) <expr>)
    | (<op1> <expr>)
    | (<op2> <expr> <expr>)
    | (set! <name> <expr>)
    | (if <expr> <expr> <expr>)
    | (block <expr>+)
    | (loop <expr>)
    | (break <expr>)
    | (<name> <expr>*)
    | (tuple <expr>+) (new)
    | (index <expr> <expr>) (new)
    | nil (new)

<op1> := add1 | sub1 | isnum | isbool | print
<op2> := + | - | * | < | > | >= | <= | =

<binding> := (<identifier> <expr>)
```

### Abstract Syntax (in Rust)

```rust
enum Op1 {
    Add1,
    Sub1,
    // Neg,
    IsNum,
    IsBool,
}

enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

enum Expr {
    Number(i64),
    True,
    False,
    Input,
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    Block(Vec<Expr>),
    Print(Box<Expr>),
    Call(String, Vec<Expr>),

    Tuple(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Nil,
}

enum Def {
    Fun(String, Vec<String>, Expr),
}

struct Prog {
    defs: Vec<Def>,
    main: Expr,
}

```

### Data Representations

`[....]` represents the last hex digit in binary.

|Data|Representation|
|:-:|:-:|
|Numbers|`0x........ .......[...0]`|
|True|`0x00000000 0000000[0111]`|
|False|`0x00000000 0000000[0011]`|
|Tuple|`0x........ .......[..01]`|
|nil|`0x00000000 0000000[0001]`|

#### Tuple structure in heap

```plain
(tuple val1 val2 ...)
```

`[size, val1, val2, ...]`

## Usage

Create a `.snek` file in the folder `tests`, e.g., `tests/example.snek`.

```plain
(fun (fact sofar n) (
    if
    (= n 1)
    sofar
    (fact (* sofar n) (+ n -1))
))

(fact 1 input)
```

This sample code computes `input!`, where `3!=3*2*1=6`.

### Compile to assembly

```bash
make tests/example.s
```

The assembly code is generated in `tests/example.s`.

### Compile to executable binary

```bash
make tests/example.run
```

The executable is generated in `tests/example.run`.

### Run the executable

```bash
# 10 is the input value, default is "false"
./tests/example.run 10
# output: 3628800
```

## Testing

Write test files (`.snek` files) in the `tests` directory, add entries in `tests/all_tests.rs`, and then run:

```bash
make test
```
