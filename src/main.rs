#![feature(box_syntax, box_patterns)]

#[macro_use]
extern crate combine;
extern crate combine_language;
#[macro_use]
extern crate error_chain;

mod ast;
mod parse;
mod sem;
mod compiler;
mod vm;

use std::io::Write;
use sem::Context;

fn main() {
    let input = match ::std::env::args().nth(1) {
        None => {
            writeln!(&mut std::io::stderr(), "no input given").unwrap();
            ::std::process::exit(1);
        }
        Some(input) => input,
    };
    let nodes = match parse::parse(&input) {
        Ok(nodes) => nodes,
        Err(err) => {
            writeln!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    println!("parsed: {:?}", nodes);

    let mut semctx = Context::new();
    let nodes = match semctx.check(&nodes) {
        Ok(nts) => nts,
        Err(err) => {
            writeln!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    println!("nodes: {:?}", nodes);
    println!("semantic context: {:?}", semctx);

    let instrs = compiler::compile(&nodes);

    println!("compiled: {:?}", instrs);
    // vm::eval_expression(&checked);
}
