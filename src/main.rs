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

    let (filename, contents) = match ::std::env::args().nth(1) {
        None => {
            let mut contents = String::new();
            try_or_exit!(::std::io::stdin().read_to_string(&mut contents));
            ("<stdin>".to_string(), contents)
        }
        Some(filename) => {
            let mut file = try_or_exit!(File::open(&filename));
            let mut contents = String::new();
            try_or_exit!(file.read_to_string(&mut contents));
            (filename, contents)
        }
    };
    let nodes = try_or_exit!(syntax::parse(&contents));
    debug!("nodes: {:?}", nodes);
    let prog = match ctx.transform(nodes) {
        Ok(prog) => prog,
        Err(err) => {
            let mut stderr = ::std::io::stderr();
            writeln!(stderr,
                     "{}:{}:{}: {}",
                     filename,
                     err.span.start.line.0 + 1,
                     err.span.start.column.0,
                     err.value)
                    .unwrap();
            if let Some(line) = err.span.getline(&contents) {
                writeln!(stderr, "    {}", line).unwrap();
            }
            ::std::process::exit(1);
        }
    };
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
