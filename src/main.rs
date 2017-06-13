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
use std::process::Command;
use std::path;

use basis::pos::Source;

use sem::Context;
use sem::infer::Infer;
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

fn find_runtime_library() -> Result<String, &'static str> {
    let this_binary = path::PathBuf::from(::std::env::args().next().unwrap());
    let this_binary_dir = this_binary.parent().unwrap_or(".".as_ref());
    let candidate = this_binary_dir.join("libminivm_rt.a");
    if candidate.exists() {
        return Ok(candidate.to_string_lossy().into());
    }
    Err("runtime library not found")
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
    let mut inferer = Infer::new();
    try_or_exit!(inferer
                     .infer_program(&nodes)
                     .map_err(|err| err.with_source(&source)));
    let prog = try_or_exit!(ctx.check_and_transform(nodes)
                                .map_err(|err| err.with_source(&source)));
    debug!("program: {:?}", prog);
    let module = try_or_exit!(compiler.compile_program(&prog));
    module.dump();
    match module.emit_object() {
        Ok(obj) => {
            let mut f = File::create("/tmp/module.o").unwrap();
            f.write_all(&obj).unwrap();
            let rt = try_or_exit!(find_runtime_library());
            let executable = source.stem();
            let status = Command::new("cc")
                .args(&["-o",
                        &executable,
                        "/tmp/module.o",
                        &rt,
                        "-lSystem",
                        "-lresolv",
                        "-lc",
                        "-lm"])
                .status()
                .expect("faild to execute linker cc");
            if !status.success() {
                writeln!(::std::io::stderr(),
                         "failed to link executable with error status {}",
                         status.code().unwrap_or(1))
                        .unwrap();
                ::std::process::exit(1);
            }
        }
        Err(err) => println!("{}", err),
    }
}
