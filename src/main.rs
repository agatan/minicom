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

use std::io::prelude::*;
use std::fs::File;
use std::error::Error;
use sem::Context;
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
    let mut machine = Machine::new();

    match ::std::env::args().nth(1) {
        None => {
            try_or_exit!(repl(&mut machine, &mut ctx));
        }
        Some(filename) => {
            let mut file = try_or_exit!(File::open(filename));
            let mut contents = String::new();
            try_or_exit!(file.read_to_string(&mut contents));
            try_or_exit!(run(&mut machine, &mut ctx, &contents));
        }
    };
}

fn run(machine: &mut Machine, ctx: &mut Context, input: &str) -> Result<Value, String> {
    let nodes = parse::parse(input).map_err(|err| format!("{}", err))?;
    // let nodes = ctx.check(&nodes).map_err(|err| format!("{}", err))?;
    let prog = ctx.transform(&nodes).map_err(|err| format!("{}", err))?;
    let instrs = compiler::compile(&prog);
    Ok(machine.run(&instrs))
}

fn repl(machine: &mut Machine, ctx: &mut Context) -> Result<(), Box<Error>> {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line == "quit" || line == "q" || line == "exit" {
                    return Ok(());
                }
                rl.add_history_entry(&line);
                match run(machine, ctx, &line) {
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
