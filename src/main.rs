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
    let node = match parse::parse_toplevel_node(&input) {
        Ok(node) => node,
        Err(err) => {
            write!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };

    debug!("parsed: {:?}", node);

    let expr = match node {
        ast::Node::Expr(e) => e,
        _ => unimplemented!(),
    };

    let typemap = sem::type_check(&expr).unwrap();
    debug!("typemap: {:?}", typemap);

    let checked = match sem::transform(&expr, &typemap) {
        Ok(checked) => checked,
        Err(err) => {
            writeln!(&mut std::io::stderr(), "{}", err).unwrap();
            ::std::process::exit(1);
        }
    };
    debug!("checked: {:?}", checked);

    let instrs = vm::compiler::compile_expression(&checked);
    debug!("compiled: {:?}", instrs);

    let v = vm::eval_expression(&checked);
    println!("evaluated: {:?}", v);
}
