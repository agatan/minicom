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
mod llvm;
mod compiler;

use std::io::prelude::*;
use std::fs::File;

use syntax::ast;

use sem::Context;
use compiler::Compiler;

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
    let mut compiler = ::Compiler::new();

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
    try_or_exit!(run(&mut compiler, &mut ctx, &contents));
}

fn run(compiler: &mut Compiler, ctx: &mut Context, input: &str) -> Result<(), String> {
    let nodes = syntax::parse(input).map_err(|err| format!("{}", err))?;
    debug!("nodes: {:?}", nodes);
    let prog = ctx.transform(nodes).map_err(|err| format!("{}", err))?;
    debug!("program: {:?}", prog);
    let module = try_or_exit!(compiler.compile_program(&prog));
    module.dump();
    match module.emit_object() {
        Ok(obj) => {
            let mut f = File::create("/tmp/module.o").unwrap();
            f.write_all(&obj).unwrap();
        }
        Err(err) => println!("{}", err),
    }
    Ok(())
}
