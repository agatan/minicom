#![feature(box_syntax, box_patterns)]

#[macro_use]
extern crate combine;
extern crate combine_language;
#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate rustyline;

use rustyline::Editor;
use rustyline::error::ReadlineError;

mod ast;
mod parse;
mod sem;
mod compiler;
mod vm;

use std::io::Write;
use sem::Context;
use vm::VM;

fn main() {
    env_logger::init().unwrap();

    let mut ctx = Context::new();
    let mut machine = VM::new();

    let input = match ::std::env::args().nth(1) {
        None => {
            repl(&mut machine, &mut ctx);
            return;
        }
        Some(input) => input,
    };
    run(&mut machine, &mut ctx, &input).unwrap();
}

fn run(machine: &mut VM, ctx: &mut Context, input: &str) -> Result<(), String> {
    let nodes = parse::parse(input).map_err(|err| format!("{}", err))?;
    let nodes = ctx.check(&nodes).map_err(|err| format!("{}", err))?;
    let instrs = compiler::compile(&nodes);
    machine.run(&instrs);
    Ok(())
}

fn repl(machine: &mut VM, ctx: &mut Context) {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                if let Err(err) = run(machine, ctx, &line) {
                    exit_error(err);
                }
            }
            Err(ReadlineError::Eof) |
            Err(ReadlineError::Interrupted) => break,
            Err(err) => exit_error(format!("{}", err)),
        }
    }
}

fn exit_error(msg: String) -> ! {
    writeln!(::std::io::stderr(), "{}", msg).unwrap();
    ::std::process::exit(1)
}
