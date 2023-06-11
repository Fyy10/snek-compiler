use std::env;
use std::fs::File;
use std::io::prelude::*;

use sexp::Atom::*;
use sexp::*;

use im::HashMap;

use regex::Regex;

// runtime error codes
const INVALID_ARGUMENT_ERROR : i64 = 1;
const OVERFLOW_ERROR : i64 = 2;
const INDEX_BOUND_ERROR : i64 = 3;

#[derive(Debug)]
enum Val {
    Reg(Reg),
    Imm(i64),
    RegOffset(Reg, i32),
}

#[derive(Debug)]
enum Reg {
    RAX,
    RBX,
    RSP,
    RDI,
    RSI,
    R15,
    RDX,
}

#[derive(Debug)]
enum Instr {
    IMov(Val, Val),
    IAdd(Val, Val),
    ISub(Val, Val),
    IMul(Val, Val),
    IAnd(Val, Val),
    IXor(Val, Val),
    ITest(Val, Val),
    IJe(String),
    IJle(String),
    IJne(String),
    IJmp(String),
    IJo(String),
    // IJz(String),
    IJnz(String),
    ICmp(Val, Val),
    ICmove(Val, Val),
    ICmovg(Val, Val),
    ICmovge(Val, Val),
    ICmovl(Val, Val),
    ICmovle(Val, Val),
    Label(String),
    // use sar instead of shr to deal with negative numbers
    ISar(Val, Val),
    // IShr(Val, Val),
    IPush(Reg),
    IPop(Reg),
    Call(String),
    Ret,
}

#[derive(Debug)]
enum Op1 {
    Add1,
    Sub1,
    // Neg,
    IsNum,
    IsBool,
}

#[derive(Debug)]
enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    StructEqual,
}

#[derive(Debug)]
struct Prog {
    defs: Vec<Def>,
    main: Expr,
}

#[derive(Debug)]
enum Def {
    Fun(String, Vec<String>, Expr),
}

#[derive(Debug)]
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

    TupleSet(Box<Expr>, Box<Expr>, Box<Expr>),
}

/// check if the expression is a function definition
fn is_def(s : &Sexp) -> bool {
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(word)), Sexp::List(_), _] if word == "fun" => true,
                _ => false,
            }
        }
        _ => false,
    }
}

fn parse_prog(s : &Sexp) -> Prog {
    println!("parse prog: {:?}", s);
    match s {
        Sexp::List(vec) => {
            let mut defs = Vec::<Def>::new();
            let mut cnt = 0;
            for expr in vec {
                cnt += 1;
                if is_def(expr) {
                    defs.push(parse_def(expr))
                } else {
                    // check if this is the last expression
                    if cnt != vec.len() as i32 {
                        panic!("Invalid: extra expression after main expression")
                    }
                    return Prog {
                        defs,
                        main: parse_expr(expr),
                    }
                }
            }
            panic!("Invalid: program has no main expr")
        }
        _ => panic!("Invalid: not a valid program")
    }
}

fn parse_args(args : &[Sexp]) -> Vec<String> {
    let mut ans = Vec::<String>::new();
    for s in args {
        match s {
            Sexp::Atom(S(arg)) if !is_reserved(arg.to_string()) => {
                if ans.contains(arg) {
                    panic!("Invalid: duplicate name for parameters")
                }
                ans.push(arg.to_string())
            }
            _ => panic!("Invalid: cannot parse args")
        }
    }
    ans
}

fn parse_def(s : &Sexp) -> Def {
    println!("parse def: {:?}", s);
    match s {
        Sexp::List(vec) => {
            match &vec[..] {
                [Sexp::Atom(S(word)), Sexp::List(names), body] if word == "fun" => {
                    match &names[..] {
                        [Sexp::Atom(S(fname)), args @ ..] =>
                            Def::Fun(fname.to_string(), parse_args(args), parse_expr(body)),
                        _ => panic!("Invalid: cannot parse function def names")
                    }
                }
                _ => panic!("Invalid: not a function definition")
            }
        }
        _ => panic!("Invalid: not a valid definition")
    }
}

fn parse_expr(s : &Sexp) -> Expr {
    println!("parse expr: {:?}", s);
    match s {
        Sexp::Atom(I(n)) => {
            let result = i64::try_from(*n);
            if result.is_err() {
                panic!("Invalid: \"{}\" cannot be parsed as i64", *n)
            }
            let val = result.unwrap();
            if val < -4611686018427387904 || val > 4611686018427387903 {
                panic!("Invalid: number out of bound")
            }
            Expr::Number(val)
        }
        Sexp::List(vec) =>
            match &vec[..] {
                [Sexp::Atom(S(op)), e] if op == "add1" =>
                    Expr::UnOp(Op1::Add1, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "sub1" =>
                    Expr::UnOp(Op1::Sub1, Box::new(parse_expr(e))),
                // [Sexp::Atom(S(op)), e] if op == "negate" =>
                //     Expr::UnOp(Op1::Neg, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isnum" =>
                    Expr::UnOp(Op1::IsNum, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e] if op == "isbool" =>
                    Expr::UnOp(Op1::IsBool, Box::new(parse_expr(e))),
                [Sexp::Atom(S(op)), e1, e2] if op == "+" =>
                    Expr::BinOp(Op2::Plus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "-" =>
                    Expr::BinOp(Op2::Minus, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "*" =>
                    Expr::BinOp(Op2::Times, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "=" =>
                    Expr::BinOp(Op2::Equal, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">" =>
                    Expr::BinOp(Op2::Greater, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == ">=" =>
                    Expr::BinOp(Op2::GreaterEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<" =>
                    Expr::BinOp(Op2::Less, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "<=" =>
                    Expr::BinOp(Op2::LessEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                [Sexp::Atom(S(op)), e1, e2] if op == "==" =>
                    Expr::BinOp(Op2::StructEqual, Box::new(parse_expr(e1)), Box::new(parse_expr(e2))),
                // let
                // (let ((x 10)) (add1 x))
                // (let ((x 10) (y 2)) (+ x y))
                [Sexp::Atom(S(op)), binding, body] if op == "let" =>
                    Expr::Let(parse_bind(binding), Box::new(parse_expr(body))),
                // (if cond_expr then_expr else_expr)
                [Sexp::Atom(S(word)), cond_expr, then_expr, else_expr] if word == "if" =>
                    Expr::If(Box::new(parse_expr(cond_expr)), Box::new(parse_expr(then_expr)), Box::new(parse_expr(else_expr))),
                // (loop expr)
                [Sexp::Atom(S(word)), e] if word == "loop" =>
                    Expr::Loop(Box::new(parse_expr(e))),
                // (break expr)
                [Sexp::Atom(S(word)), e] if word == "break" =>
                    Expr::Break(Box::new(parse_expr(e))),
                // (set! name expr)
                [Sexp::Atom(S(word)), Sexp::Atom(S(name)), e] if word == "set!" =>
                    Expr::Set(name.to_string(), Box::new(parse_expr(e))),
                // (block vec![expr..])
                [Sexp::Atom(S(word)), exprs @ ..] if word == "block" =>
                    Expr::Block(parse_block(exprs)),
                // (print expr)
                [Sexp::Atom(S(word)), expr] if word == "print" =>
                    Expr::Print(Box::new(parse_expr(expr))),
                // (fname args...)
                [Sexp::Atom(S(fname)), args @ ..] if !is_reserved(fname.to_string()) => {
                    let mut arg_exprs = Vec::<Expr>::new();
                    for arg in args {
                        arg_exprs.push(parse_expr(arg));
                    }
                    Expr::Call(fname.to_string(), arg_exprs)
                }
                // (tuple vec![expr..])
                [Sexp::Atom(S(word)), exprs @ ..] if word == "tuple" =>
                    Expr::Tuple(parse_tuple(exprs)),
                // (index var_expr idx_expr)
                [Sexp::Atom(S(word)), var_expr, idx_expr] if word == "index" =>
                    Expr::Index(Box::new(parse_expr(var_expr)), Box::new(parse_expr(idx_expr))),
                // (tuple-set! tup_expr idx_expr val_expr)
                [Sexp::Atom(S(word)), tup_expr, idx_expr, val_expr] if word == "tuple-set!" =>
                    Expr::TupleSet(Box::new(parse_expr(tup_expr)), Box::new(parse_expr(idx_expr)), Box::new(parse_expr(val_expr))),
                _ => panic!("Invalid: parse error")
            }
        // variable or keyword
        Sexp::Atom(S(s)) => {
            // s can be "true", "false", "input", or variable name
            // variable bindings will not reach this part so we can directly parse booleans and input
            match &s[..] {
                "true" => Expr::True,
                "false" => Expr::False,
                "input" => Expr::Input,
                "nil" => Expr::Nil,
                _ => {
                    check_valid_and_not_reserved(s.to_string());
                    Expr::Id(s.to_string())
                }
            }
        }
        _ => panic!("Invalid: parse error")
    }
}

/// check if variable name is reserved, return true if it is
fn is_reserved(name : String) -> bool {
    let names = [
        "add1",
        "sub1",
        // "negate",
        "isnum",
        "isbool",
        "+",
        "-",
        "*",
        "<",
        ">",
        "<=",
        ">=",
        "=",
        "==",
        "let",
        "true",
        "false",
        "input",
        "set!",
        "if",
        "block",
        "loop",
        "break",
        "print",
        "fun",
        "tuple",
        "index",
        "nil",
        "tuple-set!",
    ];
    if names.contains(&name.as_str()) {
        return true;
    }
    return false;
}

/// check if variable name is valid
fn is_valid(name : String) -> bool {
    let reg = Regex::new(r"^[a-zA-Z][a-zA-Z0-9]*").unwrap();
    reg.is_match(name.as_str())
}

/// panic if name is invalid or reserved
fn check_valid_and_not_reserved(name : String) {
    if !is_valid(name.clone()) {
        panic!("Invalid: invalid variable name {name}")
    }
    if is_reserved(name.clone()) {
        panic!("Invalid: variable name {name} is reserved for keyword")
    }
}

/// parse binding
fn parse_bind(s : &Sexp) -> Vec<(String, Expr)> {
    println!("{:?}", s);
    match s {
        // ((x 10) (y 2))
        Sexp::List(vec) => {
            let mut ans = Vec::<(String, Expr)>::new();
            if vec.len() == 0 {
                panic!("Invalid: no binding")
            }
            for bind in vec {
                match bind {
                    Sexp::List(vec) => {
                        match &vec[..] {
                            // (x 10)
                            [Sexp::Atom(S(name)), e] => {
                                // check valid name
                                check_valid_and_not_reserved(name.to_string());
                                ans.push((name.to_string(), parse_expr(e)));
                            }
                            _ => panic!("Invalid: binding parse error")
                        }
                    }
                    _ => panic!("Invalid: binding parse error")
                }
            }
            return ans
        }
        _ => panic!("Invalid: binding parse error")
    }
}

fn parse_block(exprs : &[Sexp]) -> Vec<Expr> {
    let mut ans = Vec::<Expr>::new();
    for e in exprs {
        ans.push(parse_expr(e));
    }
    if ans.len() == 0 {
        panic!("Invalid: block has no expression")
    }
    ans
}

fn parse_tuple(exprs : &[Sexp]) -> Vec<Expr> {
    let mut ans = Vec::<Expr>::new();
    for e in exprs {
        ans.push(parse_expr(e));
    }
    if ans.len() == 0 {
        panic!("Invalid: empty tuple")
    }
    ans
}

/// return a new label "{s}_{l}"
fn new_label(l : &mut i32, s : &str) -> String {
    let current = *l;
    *l += 1;
    format!("{s}_{current}")
}

/// check if rax is a number, if not, call throw_error with INVALID_ARGUMENT_ERROR
fn check_rax_isnum_instrs() -> Vec<Instr> {
    // check value
    // mov rsi, {INVALID_ARGUMENT_ERROR}
    // test rax, 0b01
    // jnz throw_error
    let mut ans = Vec::<Instr>::new();
    ans.append(&mut vec![
        Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)),
        Instr::ITest(Val::Reg(Reg::RAX), Val::Imm(0b01)),
        Instr::IJnz("throw_error".to_string()),
    ]);
    ans
}

/// check if rax is an address, if not, call throw_error with INVALID_ARGUMENT_ERROR
fn check_rax_isaddr_instrs() -> Vec<Instr> {
    // check tag
    // mov rsi, {INVALID_ARGUMENT_ERROR}
    // mov rbx, rax
    // and rbx, 0b11
    // cmp rbx, 1
    // jne throw_error
    let mut ans = Vec::<Instr>::new();
    ans.append(&mut vec![
        Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)),
        Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
        Instr::IAnd(Val::Reg(Reg::RBX), Val::Imm(0b11)),
        Instr::ICmp(Val::Reg(Reg::RBX), Val::Imm(1)),
        Instr::IJne("throw_error".to_string()),
    ]);
    ans
}

/// check if there is an overflow, if so, call throw_error with OVERFLOW_ERROR
fn check_overflow_instrs() -> Vec<Instr> {
    // mov rsi, {OVERFLOW_ERROR}
    // jo throw_error
    let mut ans = Vec::<Instr>::new();
    ans.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(OVERFLOW_ERROR)));
    ans.push(Instr::IJo("throw_error".to_string()));
    ans
}

/// compile function definitions to vector of Instr
fn compile_def_to_instrs(d : &Def, label : &mut i32, fnames : &mut HashMap<String, i32>) -> Vec<Instr> {
    match d {
        Def::Fun(fname, args, body) => {
            // {fname}:
            //   {body_instrs}
            //   ret
            let mut ans = vec![Instr::Label(fname.to_string())];
            let mut body_env = HashMap::<String, i32>::new();
            let mut arg_cnt = 1;
            for arg in args {
                body_env.insert(arg.to_string(), -8 * arg_cnt);
                arg_cnt += 1;
            }
            // input can also be considered a param, but should not present in function body
            // if env contains "input", we know that we are compiling a function body
            body_env.insert("input".to_string(), -8 * arg_cnt);
            let mut body_instrs = compile_to_instrs(body, 2, &body_env, &fnames, &String::new(), label);
            ans.append(&mut body_instrs);
            ans.push(Instr::Ret);
            ans
        }
    }
}

fn compile_to_instrs(e : &Expr, si : i32, env : &HashMap<String, i32>, fnames : &HashMap<String, i32>, break_target : &String, label : &mut i32) -> Vec<Instr> {
    println!("compile to instrs: {:?}", e);
    match e {
        Expr::Number(n) => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(*n << 1))],
        Expr::True => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b111))],
        Expr::False => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b011))],
        Expr::Input => {
            // if env contains "input", then we are compiling a function body
            // function body should not contain "input"
            if env.contains_key("input") {
                panic!("Invalid: \"input\" is used in function definition")
            }
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RDI))]
        }
        Expr::UnOp(Op1::Add1, subexpr) => {
            // compile_expr(subexpr, si, env) + "\n  add rax, 1",
            let mut ans = compile_to_instrs(subexpr, si, env, fnames, break_target, label);
            // check value
            ans.append(&mut check_rax_isnum_instrs());
            // note that 1 is represented as 0b10 now
            ans.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(0b10)));
            // check overflow
            ans.append(&mut check_overflow_instrs());
            ans
        }
        Expr::UnOp(Op1::Sub1, subexpr) => {
            // compile_expr(subexpr, si, env) + "\n  sub rax, 1",
            let mut ans = compile_to_instrs(subexpr, si, env, fnames, break_target, label);
            // check value
            ans.append(&mut check_rax_isnum_instrs());
            // note that 1 is represented as 0b10 now
            ans.push(Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(0b10)));
            // check overflow
            ans.append(&mut check_overflow_instrs());
            ans
        }
        // Expr::UnOp(Op1::Neg, subexpr) =>
        //     compile_expr(subexpr, si, env) + "\n  neg rax",
        // isnum
        Expr::UnOp(Op1::IsNum, subexpr) => {
            // check the tag, numbers have 0b0 in the last bit
            // mov rax, <subexpr>
            // and rax, 1
            // cmp rax, 1
            // mov rbx, 0b11
            // mov rax, 0b111
            // cmove rax, rbx
            let mut ans = compile_to_instrs(subexpr, si, env, fnames, break_target, label);
            ans.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(1)));
            ans.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(1)));
            ans.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b11)));
            ans.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b111)));
            ans.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            ans
        }
        // isbool
        Expr::UnOp(Op1::IsBool, subexpr) => {
            // check the tag, booleans have 0b11 in the last bits
            // mov rax, <subexpr>
            // and rax, 0b11
            // cmp rax, 0b11
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmove rax, rbx
            let mut ans = compile_to_instrs(subexpr, si, env, fnames, break_target, label);
            ans.push(Instr::IAnd(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            ans.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            ans.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            ans.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            ans.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            ans
        }
        // +
        Expr::BinOp(Op2::Plus, e1, e2) => {
            // let e1_instrs = compile_expr(e1, si, env);
            // let e2_instrs = compile_expr(e2, si + 1, env);
            // let stack_offset = si * 8;
            // format!("
            //     {e1_instrs}
            //     {check_rax_isnum_instrs}
            //     mov [rsp - {stack_offset}], rax
            //     {e2_instrs}
            //     {check_rax_isnum_instrs}
            //     add rax, [rsp - {stack_offset}]
            //     {check_overflow_instrs}
            // ")
            let mut e1_instrs = compile_to_instrs(e1, si, env, fnames, break_target, label);
            let mut e2_instrs = compile_to_instrs(e2, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e1_instrs.append(&mut check_rax_isnum_instrs());
            e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e1_instrs.append(&mut e2_instrs);
            e1_instrs.append(&mut check_rax_isnum_instrs());
            e1_instrs.push(Instr::IAdd(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e1_instrs.append(&mut check_overflow_instrs());
            e1_instrs
        }
        // -
        // (- a b) = a - b
        Expr::BinOp(Op2::Minus, e1, e2) => {
            // e2 goes first
            // let e1_instrs = compile_expr(e1, si + 1, env);
            // let e2_instrs = compile_expr(e2, si, env);
            // let stack_offset = si * 8;
            // format!("
            //     {e2_instrs}
            //     {check_rax_isnum_instrs}
            //     mov [rsp - {stack_offset}], rax
            //     {e1_instrs}
            //     {check_rax_isnum_instrs}
            //     sub rax, [rsp - {stack_offset}]
            //     {check_overflow_instrs}
            // ")
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, fnames, break_target, label);
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e2_instrs.append(&mut e1_instrs);
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::ISub(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e2_instrs.append(&mut check_overflow_instrs());
            e2_instrs
        }
        // *
        Expr::BinOp(Op2::Times, e1, e2) => {
            // let e1_instrs = compile_expr(e1, si, env);
            // let e2_instrs = compile_expr(e2, si + 1, env);
            // let stack_offset = si * 8;
            // format!("
            //     {e1_instrs}
            //     {check_rax_isnum_instrs}
            //     mov [rsp - {stack_offset}], rax
            //     {e2_instrs}
            //     {check_rax_isnum_instrs}
            //     sar rax, 1
            //     imul rax, [rsp - {stack_offset}]
            //     {check_overflow_instrs}
            // ")
            let mut e1_instrs = compile_to_instrs(e1, si, env, fnames, break_target, label);
            let mut e2_instrs = compile_to_instrs(e2, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e1_instrs.append(&mut check_rax_isnum_instrs());
            e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e1_instrs.append(&mut e2_instrs);
            e1_instrs.append(&mut check_rax_isnum_instrs());
            e1_instrs.push(Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)));
            e1_instrs.push(Instr::IMul(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e1_instrs.append(&mut check_overflow_instrs());
            e1_instrs
        }
        // =
        Expr::BinOp(Op2::Equal, e1, e2) => {
            let mut e1_instrs = compile_to_instrs(e1, si, env, fnames, break_target, label);
            let mut e2_instrs = compile_to_instrs(e2, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            // {e1_instrs}
            // mov [rsp - stack_offset], rax
            // {e2_instrs}
            // mov rbx, rax
            // xor rbx, [rsp - stack_offset]
            // test rbx, 1
            // mov rsi, {INVALID_ARGUMENT_ERROR}
            // jne throw_error
            // cmp rax, [rsp - stack_offset]
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmove rax, rbx
            e1_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e1_instrs.append(&mut e2_instrs);
            e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            e1_instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            e1_instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            e1_instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)));
            e1_instrs.push(Instr::IJne("throw_error".to_string()));
            e1_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e1_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            e1_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            e1_instrs.push(Instr::ICmove(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e1_instrs
        }
        // >
        Expr::BinOp(Op2::Greater, e1, e2) => {
            // {e2_instrs}
            // {check_rax_isnum_instrs}
            // mov [rsp - {stack_offset}], rax
            // {e1_instrs}
            // {check_rax_isnum_instrs}

            // unnecessary
            // mov rbx, rax
            // xor rbx, [rsp - {stack_offset}]
            // test rbx, 1
            // mov rsi, {INVALID_ARGUMENT_ERROR}
            // jne throw_error

            // cmp rax, [rsp - {stack_offset}]
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmovg rax, rbx
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e2_instrs.append(&mut e1_instrs);
            e2_instrs.append(&mut check_rax_isnum_instrs());
            // checking same type is unnecessary, since we check that both should be numbers
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            // e2_instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            // e2_instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)));
            // e2_instrs.push(Instr::IJne("throw_error".to_string()));
            e2_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            e2_instrs.push(Instr::ICmovg(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e2_instrs
        }
        // >=
        Expr::BinOp(Op2::GreaterEqual, e1, e2) => {
            // {e2_instrs}
            // {check_rax_isnum_instrs}
            // mov [rsp - {stack_offset}], rax
            // {e1_instrs}
            // {check_rax_isnum_instrs}

            // unnecessary
            // mov rbx, rax
            // xor rbx, [rsp - {stack_offset}]
            // test rbx, 1
            // mov rsi, {INVALID_ARGUMENT_ERROR}
            // jne throw_error

            // cmp rax, [rsp - {stack_offset}]
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmovge rax, rbx
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e2_instrs.append(&mut e1_instrs);
            e2_instrs.append(&mut check_rax_isnum_instrs());
            // checking same type is unnecessary, since we check that both should be numbers
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            // e2_instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            // e2_instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)));
            // e2_instrs.push(Instr::IJne("throw_error".to_string()));
            e2_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            e2_instrs.push(Instr::ICmovge(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e2_instrs
        }
        // <
        Expr::BinOp(Op2::Less, e1, e2) => {
            // {e2_instrs}
            // {check_rax_isnum_instrs}
            // mov [rsp - {stack_offset}], rax
            // {e1_instrs}
            // {check_rax_isnum_instrs}

            // unnecessary
            // mov rbx, rax
            // xor rbx, [rsp - {stack_offset}]
            // test rbx, 1
            // mov rsi, {INVALID_ARGUMENT_ERROR}
            // jne throw_error

            // cmp rax, [rsp - {stack_offset}]
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmovl rax, rbx
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e2_instrs.append(&mut e1_instrs);
            e2_instrs.append(&mut check_rax_isnum_instrs());
            // checking same type is unnecessary, since we check that both should be numbers
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            // e2_instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            // e2_instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)));
            // e2_instrs.push(Instr::IJne("throw_error".to_string()));
            e2_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            e2_instrs.push(Instr::ICmovl(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e2_instrs
        }
        // <=
        Expr::BinOp(Op2::LessEqual, e1, e2) => {
            // {e2_instrs}
            // {check_rax_isnum_instrs}
            // mov [rsp - {stack_offset}], rax
            // {e1_instrs}
            // {check_rax_isnum_instrs}

            // unnecessary
            // mov rbx, rax
            // xor rbx, [rsp - {stack_offset}]
            // test rbx, 1
            // mov rsi, {INVALID_ARGUMENT_ERROR}
            // jne throw_error

            // cmp rax, [rsp - {stack_offset}]
            // mov rbx, 0b111
            // mov rax, 0b11
            // cmovle rax, rbx
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);
            let mut e1_instrs = compile_to_instrs(e1, si + 1, env, fnames, break_target, label);
            let stack_offset = si * 8;
            e2_instrs.append(&mut check_rax_isnum_instrs());
            e2_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e2_instrs.append(&mut e1_instrs);
            e2_instrs.append(&mut check_rax_isnum_instrs());
            // checking same type is unnecessary, since we check that both should be numbers
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)));
            // e2_instrs.push(Instr::IXor(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, stack_offset)));
            // e2_instrs.push(Instr::ITest(Val::Reg(Reg::RBX), Val::Imm(1)));
            // e2_instrs.push(Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INVALID_ARGUMENT_ERROR)));
            // e2_instrs.push(Instr::IJne("throw_error".to_string()));
            e2_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RBX), Val::Imm(0b111)));
            e2_instrs.push(Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            e2_instrs.push(Instr::ICmovle(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)));
            e2_instrs
        }
        // ==
        Expr::BinOp(Op2::StructEqual, e1, e2) => {
            // {e1_instrs}
            // {check_rax_isaddr_instrs}
            // mov rdx, rdi     ; rdx stores input
            // mov rdi, rax
            // {e2_instrs}
            // {check_rax_isaddr_instrs}
            // mov rsi, rax
            // sub rsp, {offset}
            // push rdx
            // call snek_equal
            // pop rdi
            // add rsp, {offset}

            let idx;
            // if si is odd, add 1 before *8 (16-byte aligned)
            if si & 1 == 1 {
                idx = si + 1;
            } else {
                idx = si;
            }
            let offset = idx * 8;

            let mut e1_instrs = compile_to_instrs(e1, si, env, fnames, break_target, label);
            // results are not in the stack, we don't need si + 1 here
            let mut e2_instrs = compile_to_instrs(e2, si, env, fnames, break_target, label);

            let mut ans = vec![];
            ans.append(&mut e1_instrs);
            ans.append(&mut check_rax_isaddr_instrs());
            ans.append(&mut vec![
                Instr::IMov(Val::Reg(Reg::RDX), Val::Reg(Reg::RDI)),
                Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)),
            ]);
            ans.append(&mut e2_instrs);
            ans.append(&mut check_rax_isaddr_instrs());
            ans.append(&mut vec![
                Instr::IMov(Val::Reg(Reg::RSI), Val::Reg(Reg::RAX)),
                Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
                Instr::IPush(Reg::RDX),
                Instr::Call("snek_equal".to_string()),
                Instr::IPop(Reg::RDI),
                Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
            ]);
            ans
        }
        // let: variable name to value
        Expr::Id(name) => {
            // check if variable has been defined
            if !env.contains_key(name) {
                panic!("Unbound variable identifier {name}")
            }
            let stack_offset = env.get(name).copied().unwrap();
            // format!("
            //     mov rax, [rsp - {stack_offset}]
            // ")
            vec![Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, stack_offset))]
        }
        // let ((x 10) (y 1)) (x + y)
        Expr::Let(vec, body) => {
            // stack index for binding variables
            let mut bind_si = si;
            // instructions for saving variable bindings
            // let mut bind_instrs = String::new();
            let mut bind_instrs = Vec::<Instr>::new();
            // variable name -> stack offset
            let mut nenv = env.clone();
            // variables defined in this bind list
            let mut current_binding = Vec::<String>::new();

            for bind_expr in vec {
                let name = &bind_expr.0;
                let expr = &bind_expr.1;
                // let e_instrs = compile_expr(expr, bind_si, &nenv);
                let mut e_instrs = compile_to_instrs(expr, bind_si, &nenv, fnames, break_target, label);

                // check duplicate variable name
                if current_binding.contains(&name.to_string()) {
                    panic!("Duplicate binding")
                }
                current_binding.push(name.to_string());
                let stack_offset = bind_si * 8;
                nenv.insert(name.to_string(), stack_offset);

                // bind_instrs += &format!("
                //     {e_instrs}
                //     mov [rsp - {stack_offset}], rax
                // ").to_string();
                bind_instrs.append(&mut e_instrs);
                bind_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));

                bind_si += 1;
            }

            // let body_instrs = compile_expr(body, bind_si, &nenv);
            let mut body_instrs = compile_to_instrs(body, bind_si, &nenv, fnames, break_target, label);

            // format!("{bind_instrs}{body_instrs}")
            bind_instrs.append(&mut body_instrs);
            bind_instrs
        }
        // (if <cond_expr> <then_expr> <else_expr>)
        Expr::If(cond, thn, els) => {
            // {cond_instrs}
            // cmp rax, 0b11
            // je {else_label}
            //   {thn_instrs}
            //   jmp {end_label}
            // {else_label}:
            //   {els_instrs}
            // {end_label}:
            let end_label = new_label(label, "ifend");
            let else_label = new_label(label, "ifelse");
            let mut cond_instrs = compile_to_instrs(cond, si, env, fnames, break_target, label);
            let mut thn_instrs = compile_to_instrs(thn, si, env, fnames, break_target, label);
            let mut els_instrs = compile_to_instrs(els, si, env, fnames, break_target, label);
            cond_instrs.push(Instr::ICmp(Val::Reg(Reg::RAX), Val::Imm(0b11)));
            cond_instrs.push(Instr::IJe(else_label.clone()));
            cond_instrs.append(&mut thn_instrs);
            cond_instrs.push(Instr::IJmp(end_label.clone()));
            cond_instrs.push(Instr::Label(else_label.clone()));
            cond_instrs.append(&mut els_instrs);
            cond_instrs.push(Instr::Label(end_label.clone()));
            cond_instrs
        }
        // (loop <expr>)
        Expr::Loop(e) => {
            // {loop_label}:
            //   {expr_instrs}
            //   jmp {loop_label}
            // {endloop_label}:
            let loop_label = new_label(label, "loop");
            let endloop_label = new_label(label, "endloop");
            let mut e_instrs = compile_to_instrs(e, si, env, fnames, &endloop_label, label);
            let mut ans = vec![Instr::Label(loop_label.clone())];
            ans.append(&mut e_instrs);
            ans.push(Instr::IJmp(loop_label.clone()));
            ans.push(Instr::Label(endloop_label.clone()));
            ans
        }
        // (break <expr>)
        Expr::Break(e) => {
            // {expr_instrs}
            // jmp {break_target}
            let mut e_instrs = compile_to_instrs(e, si, env, fnames, break_target, label);
            if break_target.is_empty() {
                panic!("Error: no break target (no surrounding loop)")
            }
            e_instrs.push(Instr::IJmp(break_target.to_string()));
            e_instrs
        }
        // (set! <name> <expr>)
        Expr::Set(name, expr) => {
            // {expr_instrs}
            // mov [rsp - {stack_offset}], rax
            let mut e_instrs = compile_to_instrs(expr, si, env, fnames, break_target, label);
            // check if variable has been defined
            if !env.contains_key(name) {
                panic!("Unbound variable identifier {name}")
            }
            let stack_offset = env.get(name).copied().unwrap();
            e_instrs.push(Instr::IMov(Val::RegOffset(Reg::RSP, stack_offset), Val::Reg(Reg::RAX)));
            e_instrs
        }
        // (block vec![<expr>])
        Expr::Block(exprs) => {
            // {e1_instrs}
            // {e2_instrs}
            // ...
            let mut ans = Vec::<Instr>::new();
            for e in exprs {
                ans.append(&mut compile_to_instrs(e, si, env, fnames, break_target, label));
            }
            ans
        }
        // (print expr)
        Expr::Print(expr) => {
            // {e_instrs}
            // sub rsp, {offset}
            // push rdi
            // mov rdi, rax
            // mov rbx, rax
            // call snek_print
            // mov rax, rbx
            // pop rdi
            // add rsp, {offset}

            let idx;
            // if si is odd, add 1 before *8 (16-byte aligned)
            if si & 1 == 1 {
                idx = si + 1;
            } else {
                idx = si;
            }
            let offset = idx * 8;
            let mut ans = compile_to_instrs(expr, si, env, fnames, break_target, label);
            ans.append(
                &mut vec![
                    Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
                    // 16-byte aligned
                    Instr::IPush(Reg::RDI),
                    Instr::IMov(Val::Reg(Reg::RDI), Val::Reg(Reg::RAX)),
                    Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                    Instr::Call("snek_print".to_string()),
                    Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::RBX)),
                    Instr::IPop(Reg::RDI),
                    Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
                ]
            );
            ans
        }
        // (fname args...)
        Expr::Call(fname, arg_exprs) => {
            if !fnames.contains_key(fname) {
                panic!("Invalid: undefined function {fname}")
            }
            let defined_arg_cnt = *fnames.get(fname).unwrap();
            if defined_arg_cnt != arg_exprs.len() as i32 {
                panic!("Invalid: {fname} requires {} arg(s) but got {}", defined_arg_cnt, arg_exprs.len());
            }
            // note that rsp should be 16-byte aligned "after" call
            // which should end with 0x8 "before" call
            // {arg1_instrs}
            // mov [rsp - (offset)], rax
            // {arg2_instrs}
            // mov [rsp - (offset - 8)], rax
            // {arg3_instrs}
            // mov [rsp - (offset - 16)], rax
            // ...
            // mov [rsp - (offset - argc * 8)], rdi
            // sub rsp, {offset}
            // call {fname}
            // mov rdi, [rsp + argc * 8]
            // add rsp, {offset}
            let idx;
            let argc = arg_exprs.len() as i32;
            if (si + argc) & 1 == 1 {
                idx = si + argc;
            } else {
                idx = si + argc + 1;
            }
            // idx is odd
            let offset = idx * 8;
            let mut ans = Vec::<Instr>::new();
            let mut i = 0;
            for expr in arg_exprs {
                let mut arg_instrs = compile_to_instrs(expr, idx + 1, env, fnames, break_target, label);
                ans.append(&mut arg_instrs);
                ans.push(Instr::IMov(Val::RegOffset(Reg::RSP, offset - i * 8), Val::Reg(Reg::RAX)));
                i += 1;
            }
            ans.append(
                &mut vec![
                    Instr::IMov(Val::RegOffset(Reg::RSP, offset - argc * 8), Val::Reg(Reg::RDI)),
                    Instr::ISub(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
                    Instr::Call(fname.to_string()),
                    Instr::IMov(Val::Reg(Reg::RDI), Val::RegOffset(Reg::RSP, -argc * 8)),
                    Instr::IAdd(Val::Reg(Reg::RSP), Val::Imm(offset as i64)),
                ]
            );
            ans
        }
        // (tuple vec![<expr>])
        Expr::Tuple(exprs) => {
            // store to stack
            // {e1_instrs}
            // mov [rsp - {(si+0) * 8}], rax
            // {e2_instrs}
            // mov [rsp - {(si+1) * 8}], rax
            // {e3_instrs}
            // mov [rsp - {(si+2) * 8}], rax
            // ...

            // store to heap
            // mov [r15], {size}
            // mov rbx, [rsp - {(si+0) * 8}]
            // mov [r15 + 8 * 1], rbx
            // mov rbx, [rsp - {(si+1) * 8}]
            // mov [r15 + 8 * 2], rbx
            // ...

            // store addr to rax, move forward heap base
            // mov rax, r15
            // add rax, 1
            // add r15, {allocated_size * 16}

            // store elements to stack
            let size = exprs.len() as i32;
            let mut stack_bias = 0;
            let mut ans = Vec::<Instr>::new();
            for e in exprs {
                ans.append(&mut compile_to_instrs(e, si + stack_bias, env, fnames, break_target, label));
                ans.push(Instr::IMov(Val::RegOffset(Reg::RSP, (si + stack_bias) * 8), Val::Reg(Reg::RAX)));
                stack_bias += 1;
            }

            // store size to heap
            ans.push(Instr::IMov(Val::RegOffset(Reg::R15, 0), Val::Imm(size as i64)));

            // store elements to heap
            for heap_index in 1..=size {
                ans.append(&mut vec![
                    Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, (si+heap_index-1) * 8)),
                    Instr::IMov(Val::RegOffset(Reg::R15, -8 * heap_index), Val::Reg(Reg::RBX)),
                ]);
            }

            // store r15 to rax, move forward r15
            let allocated_size;
            if size & 1 == 1 {
                allocated_size = (size + 1) / 2;
            } else {
                allocated_size = size / 2 + 1;
            }
            ans.append(&mut vec![
                Instr::IMov(Val::Reg(Reg::RAX), Val::Reg(Reg::R15)),
                Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IAdd(Val::Reg(Reg::R15), Val::Imm(allocated_size as i64 * 16)),
            ]);
            ans
        }
        // (index var_expr idx_expr)
        Expr::Index(var_expr, idx_expr) => {
            // idx_expr_instrs
            // {check_rax_isnum_instrs}
            // sar rax, 1       ; rax stores idx
            // mov [rsp - {si * 8}], rax
            // var_expr_instrs
            // {check_rax_isaddr_instrs}
            // sub rax, 1       ; rax stores tuple address
            // mov rbx, [rax]   ; rbx stores tuple size
            // mov rsi, {INDEX_BOUND_ERROR}
            // cmp rbx, [rsp - {si * 8}]
            // jle throw_error
            // cmp [rsp - {si * 8}], -1
            // jle throw_error
            // mov rbx, rax     ; rbx stores tuple address
            // mov rax, [rsp - {si * 8}] ; rax stores idx
            // add rax, 1
            // imul rax, 8
            // add rbx, rax     ; rbx stores base address + offset
            // mov rax, [rbx]
            let mut ans = compile_to_instrs(idx_expr, si, env, fnames, break_target, label);
            ans.append(&mut check_rax_isnum_instrs());
            ans.append(&mut vec![
                Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)),
            ]);
            ans.append(&mut compile_to_instrs(var_expr, si + 1, env, fnames, break_target, label));
            ans.append(&mut check_rax_isaddr_instrs());
            ans.append(&mut vec![
                Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RAX, 0)),
                Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INDEX_BOUND_ERROR)),
                Instr::ICmp(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)),
                Instr::IJle("throw_error".to_string()),
                Instr::ICmp(Val::RegOffset(Reg::RSP, si * 8), Val::Imm(-1)),
                Instr::IJle("throw_error".to_string()),
                Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)),
                Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(8)),
                Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RBX, 0)),
            ]);
            ans
        }
        // nil (mov rax, 0b01)
        Expr::Nil => vec![Instr::IMov(Val::Reg(Reg::RAX), Val::Imm(0b01))],
        // (tuple-set! tup_expr idx_expr val_expr)
        Expr::TupleSet(tup_expr, idx_expr, val_expr) => {
            // idx_expr_instrs
            // {check_rax_isnum_instrs}
            // sar rax, 1       ; rax stores idx
            // mov [rsp - {si * 8}], rax
            // tup_expr_instrs
            // {check_rax_isaddr_instrs}
            // mov [rsp - {(si+1) * 8}], rax
            // sub rax, 1       ; rax stores tuple address
            // mov rbx, [rax]   ; rbx stores tuple size
            // mov rsi, {INDEX_BOUND_ERROR}
            // cmp rbx, [rsp - {si * 8}]
            // jle throw_error
            // cmp [rsp - {si * 8}], -1
            // jle throw_error
            // mov rbx, rax     ; rbx stores tuple address
            // mov rax, [rsp - {si * 8}] ; rax stores idx
            // add rax, 1
            // imul rax, 8
            // add rbx, rax     ; rbx stores base address + offset
            // mov [rsp - {si * 8}], rbx
            // {val_instrs}
            // mov rbx, [rsp - {si * 8}]
            // mov [rbx], rax
            // mov rax, [rsp - {(si+1) * 8}]
            let mut ans = compile_to_instrs(idx_expr, si, env, fnames, break_target, label);
            ans.append(&mut check_rax_isnum_instrs());
            ans.append(&mut vec![
                Instr::ISar(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RAX)),
            ]);
            ans.append(&mut compile_to_instrs(tup_expr, si + 1, env, fnames, break_target, label));
            ans.append(&mut check_rax_isaddr_instrs());
            ans.append(&mut vec![
                Instr::IMov(Val::RegOffset(Reg::RSP, (si+1) * 8), Val::Reg(Reg::RAX)),
                Instr::ISub(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RAX, 0)),
                Instr::IMov(Val::Reg(Reg::RSI), Val::Imm(INDEX_BOUND_ERROR)),
                Instr::ICmp(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)),
                Instr::IJle("throw_error".to_string()),
                Instr::ICmp(Val::RegOffset(Reg::RSP, si * 8), Val::Imm(-1)),
                Instr::IJle("throw_error".to_string()),
                Instr::IMov(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, si * 8)),
                Instr::IAdd(Val::Reg(Reg::RAX), Val::Imm(1)),
                Instr::IMul(Val::Reg(Reg::RAX), Val::Imm(8)),
                Instr::IAdd(Val::Reg(Reg::RBX), Val::Reg(Reg::RAX)),
                Instr::IMov(Val::RegOffset(Reg::RSP, si * 8), Val::Reg(Reg::RBX)),
            ]);
            ans.append(&mut compile_to_instrs(val_expr, si + 2, env, fnames, break_target, label));
            ans.append(&mut vec![
                Instr::IMov(Val::Reg(Reg::RBX), Val::RegOffset(Reg::RSP, si * 8)),
                Instr::IMov(Val::RegOffset(Reg::RBX, 0), Val::Reg(Reg::RAX)),
                Instr::IMov(Val::Reg(Reg::RAX), Val::RegOffset(Reg::RSP, (si+1) * 8)),
            ]);
            ans
        }
        // unreachable
        // _ => panic!("compile error")
    }
}

fn instr_to_str(i : &Instr) -> String {
    match i {
        Instr::IMov(v1, v2) =>
            format!("mov {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IAdd(v1, v2) =>
            format!("add {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ISub(v1, v2) =>
            format!("sub {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IMul(v1, v2) =>
            format!("imul {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IAnd(v1, v2) =>
            format!("and {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IXor(v1, v2) =>
            format!("xor {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ITest(v1, v2) =>
            format!("test {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IJe(target) =>
            format!("je {}", target),
        Instr::IJle(target) =>
            format!("jle {}", target),
        Instr::IJne(target) =>
            format!("jne {}", target),
        Instr::IJmp(target) =>
            format!("jmp {}", target),
        Instr::IJo(target) =>
            format!("jo {}", target),
        // Instr::IJz(target) =>
        //     format!("jz {}", target),
        Instr::IJnz(target) =>
            format!("jnz {}", target),
        Instr::ICmp(v1, v2) =>
            format!("cmp {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmove(v1, v2) =>
            format!("cmove {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovg(v1, v2) =>
            format!("cmovg {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovge(v1, v2) =>
            format!("cmovge {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovl(v1, v2) =>
            format!("cmovl {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::ICmovle(v1, v2) =>
            format!("cmovle {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::Label(label) =>
            format!("{}:", label),
        Instr::ISar(v1, v2) =>
            format!("sar {}, {}", val_to_str(v1), val_to_str(v2)),
        // Instr::IShr(v1, v2) =>
        //     format!("shr {}, {}", val_to_str(v1), val_to_str(v2)),
        Instr::IPush(reg) =>
            format!("push {}", reg_to_str(reg)),
        Instr::IPop(reg) =>
            format!("pop {}", reg_to_str(reg)),
        Instr::Call(fname) =>
            format!("call {}", fname),
        Instr::Ret => format!("ret"),
        // unreachable
        // _ => panic!("Invalid: no matched instruction")
    }
}

fn val_to_str(v : &Val) -> String {
    match v {
        Val::Imm(n) => format!("{}", *n),
        Val::Reg(reg) => reg_to_str(reg),
        Val::RegOffset(reg, offset) =>
            format!("QWORD [{} - {}]", reg_to_str(reg), *offset),
        // unreachable
        // _ => panic!("Invalid: no matched value")
    }
}

fn reg_to_str(r : &Reg) -> String {
    match r {
        Reg::RAX => format!("rax"),
        Reg::RBX => format!("rbx"),
        Reg::RSP => format!("rsp"),
        Reg::RDI => format!("rdi"),
        Reg::RSI => format!("rsi"),
        Reg::R15 => format!("r15"),
        Reg::RDX => format!("rdx"),
        // unreachable
        // _ => panic!("Invalid: no matched register")
    }
}

fn compile_prog(p : &Prog) -> (String, String) {
    let si = 2;
    let env = &HashMap::<String, i32>::new();
    // function name => num of args
    let mut fnames = HashMap::<String, i32>::new();
    let mut label = 0;
    let break_target = &String::new();

    // *************************
    // * compile function defs *
    // *************************
    let defs = &p.defs;
    // fill fnames (function name => num of args) before actually compiling function definitions
    for def in defs {
        match def {
            Def::Fun(fname, args, _) => {
                // check duplicate function name
                if fnames.contains_key(fname) {
                    panic!("Invalid: multiple function defined with the same name {fname}")
                }
                fnames.insert(fname.to_string(), args.len() as i32);
            }
        }
    }
    let mut defs_str = String::new();
    for def in defs {
        let def_instrs = compile_def_to_instrs(def, &mut label, &mut fnames);
        for i in def_instrs {
            match i {
                Instr::Label(_) => defs_str += &format!("{}\n", instr_to_str(&i)),
                _ => defs_str += &format!("  {}\n", instr_to_str(&i))
            }
        }
        defs_str += "\n";
    }

    // compile main expr
    let main_instrs = compile_to_instrs(&p.main, si, env, &fnames, break_target, &mut label);
    let mut main_str = String::new();
    for i in main_instrs {
        main_str += &format!("  {}\n", instr_to_str(&i));
    }

    (defs_str, main_str)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let in_name = &args[1];
    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let prog = "(".to_owned() + &in_contents + ")";
    let parsed_prog = parse(&prog);
    if parsed_prog.is_err() {
        panic!("Invalid: bad S-exp")
    }
    let prog_struct = parse_prog(&parsed_prog.unwrap());

    let (defs_str, main_str) = compile_prog(&prog_struct);
    let asm_program = format!(
        "
section .text
global our_code_starts_here
extern snek_print
extern snek_error
extern snek_equal
throw_error:
  ; store error code in rsi before calling throw_error
  ; we don't need to worry about rdi (input) being overwritten since our program ends here
  mov rdi, rsi
  push rsp
  call snek_error

; function definitions
{}
; main
our_code_starts_here:
  ; ensure heap ptr is in r15
  mov r15, rsi
{}
  ret
",
        defs_str,
        main_str
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
