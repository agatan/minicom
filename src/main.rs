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
        },
        Some(input) => input,
    };
    let expr = match parse::parse_expression(&input) {
        Ok(expr) => expr,
        Err(err) => {
            write!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    println!("expression: {:?}", expr);

    let checked = sem::transform(expr).unwrap();
    println!("checked: {:?}", checked);

    let instrs = vm::compiler::compile_expression(&checked);
    println!("compiled: {:?}", instrs);

    let v = vm::eval_expression(&checked);
    println!("evaluated: {:?}", v);
}
