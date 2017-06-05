#![feature(box_syntax, box_patterns, libc)]

#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys;
extern crate libc;

extern crate minivm_syntax as syntax;

mod sem;
mod bytecode_compiler;
mod vm;
mod llvm;
mod compiler;

use std::io::prelude::*;
use std::fs::File;

use syntax::ast;

use sem::Context;
use bytecode_compiler::Compiler;
use vm::{Value, Machine};

macro_rules! try_or_exit {
    ($x:expr) => {
        match $x {
            Ok(x) => x,
            Err(err) => {
                writeln!(::std::io::stderr(), "{}", err).unwrap();
                ::std::process::exit(1);
            }
        }
    }
}

fn main() {
    try_or_exit!(env_logger::init());

    let mut ctx = Context::new();
    let mut bcompiler = Compiler::new();
    let mut compiler = compiler::Compiler::new();
    let mut machine = Machine::new();

    let contents = match ::std::env::args().nth(1) {
        None => {
            let mut contents = String::new();
            try_or_exit!(::std::io::stdin().read_to_string(&mut contents));
            contents
        }
        Some(filename) => {
            let mut file = try_or_exit!(File::open(filename));
            let mut contents = String::new();
            try_or_exit!(file.read_to_string(&mut contents));
            contents
        }
    };
    try_or_exit!(run(&mut machine,
                     &mut bcompiler,
                     &mut compiler,
                     &mut ctx,
                     &contents));
}

fn run(machine: &mut Machine,
       bcompiler: &mut Compiler,
       compiler: &mut compiler::Compiler,
       ctx: &mut Context,
       input: &str)
       -> Result<Value, String> {
    let nodes = syntax::parse(input).map_err(|err| format!("{}", err))?;
    debug!("nodes: {:?}", nodes);
    let prog = ctx.transform(nodes).map_err(|err| format!("{}", err))?;
    debug!("program: {:?}", prog);
    let module = compiler.compile_program(&prog).unwrap();
    module.dump();
    match module.emit_object() {
        Ok(obj) => {
            let mut f = File::create("/tmp/module.o").unwrap();
            f.write_all(&obj).unwrap();
        }
        Err(err) => println!("{}", err),
    }
    let instrs = bcompiler.compile(&prog);
    debug!("instrs: {:?}", instrs);
    Ok(machine.run(bcompiler.funcs(), &instrs))
}

#[cfg(test)]
mod tests {
    use syntax;
    use sem::Context;
    use bytecode_compiler::Compiler;
    use vm::{Value, Machine};

    fn run(input: &str) -> Result<Value, String> {
        let mut machine = Machine::new();
        let mut compiler = Compiler::new();
        let mut ctx = Context::new();
        let nodes = syntax::parse(input).map_err(|err| format!("{}", err))?;
        let prog = ctx.transform(nodes).map_err(|err| format!("{}", err))?;
        let instrs = compiler.compile(&prog);
        Ok(machine.run(compiler.funcs(), &instrs))
    }

    #[test]
    fn test_primitives() {
        let tests: &[(&str, Value)] = &[(r#"0"#, Value::Int(0)),
                                        (r#" 1.0 "#, Value::Float(1.0)),
                                        (r#"1 + 1"#, Value::Int(2)),
                                        (r#"1 + 2 * 3 / 4"#, Value::Int(2))];

        for &(input, expected) in tests {
            let value = run(input).unwrap();
            assert_eq!(value, expected);
        }
    }

    #[test]
    fn test_locals() {
        let tests = &[(r#" let x: int = 0; x "#, Value::Int(0)),
                      (r#"
                            let x: int = 1;
                            let y: int = 2;
                            x + y
         "#,
                       Value::Int(3))];

        for &(input, expected) in tests {
            let value = run(input).unwrap();
            assert_eq!(value, expected);
        }
    }

    #[test]
    fn test_calls() {
        let tests = &[(r#"
                            def add(x: int, y: int): int {
                                x + y
                            };
                            add(1, 2)
         "#,
                       Value::Int(3)),
                      (r#"
                            def add(x: int, y: int): int {
                                let z = x + y;
                                z
                            };
                            add(1, 2)
         "#,
                       Value::Int(3)),
                      (r#"
                            let global: int = 0;
                            def assign(x: int, y: int) {
                                global = x + y;
                            };
                            assign(1, 2);
                            global
         "#,
                       Value::Int(3))];

        for &(input, expected) in tests {
            let value = run(input).unwrap();
            assert_eq!(value, expected);
        }
    }

    #[test]
    fn test_if() {
        let tests = &[(r#"
                            def fib(n: int): int {
                                if n <= 1 {
                                    n
                                } else {
                                    fib(n-2) + fib(n-1)
                                }
                            }
                            fib(8)
         "#,
                       Value::Int(21))];

        for &(input, expected) in tests {
            let value = run(input).unwrap();
            assert_eq!(value, expected);
        }
    }
}
