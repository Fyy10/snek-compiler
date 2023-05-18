# snek-compiler

- [snek-compiler](#snek-compiler)
  - [The Snek Language](#the-snek-language)
    - [Concrete Syntax](#concrete-syntax)
    - [Abstract Syntax (in Rust)](#abstract-syntax-in-rust)
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
    | (tuple <expr>+)
    | (index <expr> <expr>)

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
}

enum Def {
    Fun(String, Vec<String>, Expr),
}

struct Prog {
    defs: Vec<Def>,
    main: Expr,
}

```

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
```

## Testing
