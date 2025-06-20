use std::arch::asm;
use std::ffi::c_char;
use ir::VMValue;

#[unsafe(no_mangle)]
pub fn println_str(args: &[VMValue]) {
    let s = &args[0];
    match s {
        VMValue::Str(s) => {
            println!("{}", s);
        },
        _ => {
            panic!("Expected string");
        }
    }
}
#[unsafe(no_mangle)]
pub extern "C" fn println_int(i: i32) {
    println!("{}", i);
}

#[unsafe(no_mangle)]
pub extern "C" fn println_bool(i: bool) {
    println!("{}", i);
}