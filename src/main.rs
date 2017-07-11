#![feature(box_syntax, box_patterns, libc)]

#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate llvm_sys;
extern crate libc;

#[macro_use]
extern crate minicom_basis as basis;
extern crate minicom_syntax as syntax;

mod sem;
mod llvm;
mod codegen;

use std::io::prelude::*;

use error_chain::ChainedError;

use basis::sourcemap::SourceMap;

use sem::Context;
use sem::infer::Infer;
use codegen::Emitter;

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

    let mut srcmap = SourceMap::new();

    let source = match ::std::env::args().nth(1) {
        None => {
            let mut contents = String::new();
            try_or_exit!(::std::io::stdin().read_to_string(&mut contents));
            srcmap.add("<stdin>".to_string(), contents)
        }
        Some(filename) => try_or_exit!(srcmap.add_file_source(filename)),
    };
    let nodes = try_or_exit!(syntax::parse(&srcmap, &*source));
    debug!("nodes: {:?}", nodes);
    let mut inferer = Infer::new();
    try_or_exit!(inferer.infer_program(&nodes).map_err(|err| {
        err.with_source_map(&srcmap)
    }));
    let prog = try_or_exit!(ctx.check_and_transform(nodes).map_err(|err| {
        err.with_source_map(&srcmap)
    }));
    debug!("program: {:?}", prog);
    let emitter = try_or_exit!(Emitter::new(&prog).map_err(|err| err.display().to_string()));
    println!("{}", emitter.emit_llvm_ir());
    try_or_exit!(emitter.emit_executable(&source.executable_name()));
}
