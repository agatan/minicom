#![feature(box_syntax, box_patterns)]

#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate rustyline;

extern crate minivm_syntax as syntax;

use rustyline::Editor;
use rustyline::error::ReadlineError;

mod sem;
mod compiler;
mod vm;

use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

use syntax::ast;

use sem::Context;
use compiler::Compiler;
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
    let mut compiler = Compiler::new();
    let mut machine = Machine::new();

    match ::std::env::args().nth(1) {
        None => {
            try_or_exit!(repl(&mut machine, &mut compiler, &mut ctx));
        }
        Some(filename) => {
            let mut file = try_or_exit!(File::open(filename));
            let mut contents = String::new();
            try_or_exit!(file.read_to_string(&mut contents));
            try_or_exit!(run(&mut machine, &mut compiler, &mut ctx, &contents));
        }
    };
}

fn run(machine: &mut Machine,
       compiler: &mut Compiler,
       ctx: &mut Context,
       input: &str)
       -> Result<Value, String> {
    let nodes = syntax::parse(input).map_err(|err| format!("{}", err))?;
    debug!("nodes: {:?}", nodes);
    let prog = ctx.transform(&nodes).map_err(|err| format!("{}", err))?;
    debug!("program: {:?}", prog);
    let instrs = compiler.compile(ctx.root(), &prog);
    debug!("instrs: {:?}", instrs);
    Ok(machine.run(compiler.funcs(), &instrs))
}

fn repl(machine: &mut Machine,
        compiler: &mut Compiler,
        ctx: &mut Context)
        -> Result<(), Box<Error>> {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit" || line == "q" || line == "exit" {
                    return Ok(());
                }
                rl.add_history_entry(&line);
                match run(machine, compiler, ctx, &line) {
                    Ok(value) => println!("=> {}", value),
                    Err(err) => println!("{}", err),
                }
            }
            Err(ReadlineError::Eof) |
            Err(ReadlineError::Interrupted) => return Ok(()),
            Err(err) => return Err(Box::new(err)),
        }
    }
}

#[cfg(test)]
mod tests {
    use parse;
    use sem::Context;
    use compiler::Compiler;
    use vm::{Value, Machine};

    fn run(input: &str) -> Result<Value, String> {
        let mut machine = Machine::new();
        let mut compiler = Compiler::new();
        let mut ctx = Context::new();
        let nodes = parse::parse(input).map_err(|err| format!("{}", err))?;
        let prog = ctx.transform(&nodes).map_err(|err| format!("{}", err))?;
        let instrs = compiler.compile(ctx.root(), &prog);
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
                      (" let x = 0; x ", Value::Int(0)),
                      (r#"
                            let x = 1;
                            let y: int = 2;
                            x + y
         "#,
                       Value::Int(3)),
                      (r#"
                            let x = 1;
                            let y = x + 1;
                            x = x + y;
                            x
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
                            let global = 0;
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
}
