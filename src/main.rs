#![feature(box_syntax, box_patterns, libc)]

#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys;
extern crate libc;

#[macro_use]
extern crate minivm_basis as basis;
extern crate minivm_syntax as syntax;

mod sem;
mod llvm;
mod compiler;

use std::io::prelude::*;
use std::fs::File;

use basis::pos::Source;

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

    let source = match ::std::env::args().nth(1) {
        None => try_or_exit!(Source::from_stdin()),
        Some(filename) => try_or_exit!(Source::from_file(filename)),
    };
    let nodes = try_or_exit!(syntax::parse(&source));
    debug!("nodes: {:?}", nodes);
    let prog = try_or_exit!(ctx.transform(nodes).map_err(|err| err.with_source(&source)));
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
}
