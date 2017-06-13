#[no_mangle]
pub extern "C" fn print_unit(x: ()) {
    println!("()");
    x
}

#[no_mangle]
pub extern "C" fn print_int(x: i32) -> i32 {
    println!("{}", x);
    x
}

#[no_mangle]
pub extern "C" fn print_bool(x: bool) -> bool {
    println!("{}", x);
    x
}

#[no_mangle]
pub extern "C" fn print_float(x: f64) -> f64 {
    println!("{}", x);
    x
}
