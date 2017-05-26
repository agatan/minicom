#![feature(box_syntax, box_patterns)]

#[macro_use]
extern crate combine;
extern crate combine_language;
#[macro_use]
extern crate log;
extern crate env_logger;
#[macro_use]
extern crate error_chain;

mod ast;
mod parse;
mod sem;
// mod vm;

use std::io::Write;
use sem::Context;

fn main() {
    env_logger::init().unwrap();

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

    // let typemap = sem::type_check(&nodes).unwrap();
    // debug!("typemap: {:?}", typemap);
    //
    //
    // let checked = match sem::transform(&nodes, &typemap) {
    //     Ok(checked) => checked,
    //     Err(err) => {
    //         writeln!(&mut std::io::stderr(), "{}", err).unwrap();
    //         ::std::process::exit(1);
    //     }
    // };
    // debug!("checked: {:?}", checked);
    //
    // let instrs = vm::compiler::compile(&checked);
    // debug!("compiled: {:?}", instrs);
    //
    // vm::eval_expression(&checked);
}
