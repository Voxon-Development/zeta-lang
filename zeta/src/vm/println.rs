use std::arch::asm;
use std::ffi::c_char;
use ir::VMValue;
use crate::vm::memory::string_pool::StringPool;

#[unsafe(no_mangle)]
pub fn println_str(args: &[VMValue], string_pool: &StringPool) {
    let s = &args[0];
    match s {
        VMValue::Str(s) => {
            println!("{}", string_pool.resolve_string(s));
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