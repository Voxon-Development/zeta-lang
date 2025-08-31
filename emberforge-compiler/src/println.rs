use std::arch::asm;

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
pub unsafe extern "C" fn syscall6(
    syscall_number: u64,
    arg1: u64,
    arg2: u64,
    arg3: u64,
    arg4: u64,
    arg5: u64,
    arg6: u64,
) -> u64 {
    unsafe {
        let ret: u64 = 0;
        asm!(
            "mov rax, rdi",  // syscall number
            "mov rdi, rsi",  // arg1
            "mov rsi, rdx",  // arg2
            "mov rdx, rcx",  // arg3
            "mov r10, r8",   // arg4
            "mov r8,  r9",   // arg5
            "mov r9, [rsp]", // arg6 is passed on stack
            "syscall",
            "ret"
        );
        ret
    }
}
