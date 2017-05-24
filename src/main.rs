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
mod vm;

use std::io::Write;

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
            write!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    debug!("parsed: {:?}", nodes);

    let typemap = sem::type_check(&nodes).unwrap();
    debug!("typemap: {:?}", typemap);


    let checked = match sem::transform(&nodes, &typemap) {
        Ok(checked) => checked,
        Err(err) => {
            writeln!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };
    debug!("checked: {:?}", checked);

    let instrs = vm::compiler::compile_expression(&checked[0]);
    debug!("compiled: {:?}", instrs);

    vm::eval_expression(&checked[0]);
}
