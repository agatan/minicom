extern crate combine;
extern crate combine_language;

mod ast;
mod parse;

use std::io::Write;

fn main() {
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
}
