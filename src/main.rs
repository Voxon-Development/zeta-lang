use std::arch::asm;
use std::ffi::CStr;

fn main() {
    let msg = CStr::from_bytes_with_nul(b"Came from assembly!\n\0").unwrap();
    unsafe { println_syscall(msg); }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn println_syscall(string: &CStr) { 
    unsafe {
        // write syscall
        asm!(
            "syscall",
            in("rax") 1,                           // sys_write
            in("rdi") 1,                           // fd = stdout
            in("rsi") string.as_ptr(),             // buffer
            in("rdx") string.to_bytes().len(),     // length
            clobber_abi("C"),
        );
    
        // exit syscall (noreturn)
        asm!(
            "syscall",
            in("rax") 60,     // sys_exit
            in("rdi") 0,      // exit code
            options(noreturn)
        );
    }
}