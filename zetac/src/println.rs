use std::arch::asm;
use std::ffi::c_char;

#[unsafe(no_mangle)]
pub extern "C" fn println_str(s: *const u8) {
    unsafe {
        let cstr = std::ffi::CStr::from_ptr(s as *const i8);
        println!("{}", cstr.to_str().unwrap());
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

#[unsafe(no_mangle)]
pub unsafe extern "C" fn println_syscall(string: *const c_char) { 
    unsafe {
        asm!(
            "mov rax, 1",
            "mov rdi, 1",
            "mov rsi, {0}",
            "mov rdx, {1:e}",
            "syscall",
            in(reg) string,
            in(reg) std::ffi::CStr::from_ptr(string).to_bytes().len(),
            options(noreturn)
        );
    }
}