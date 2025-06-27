use ir::VMValue;

#[unsafe(no_mangle)]
pub extern "C" fn println(args: *const VMValue, len: usize) -> VMValue {
    // Safety: We trust the VM to pass valid pointers and lengths
    let args = if args.is_null() || len == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(args, len) }
    };
    
    // Use a single write to stdout for better performance
    use std::io::Write;
    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            let _ = write!(&mut handle, " ");
        }
        
        match arg {
            VMValue::Str(s) => { let _ = write!(&mut handle, "{}", s); },
            VMValue::ResolvedStr(s) => { let _ = write!(&mut handle, "{}", s); },
            VMValue::I32(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::I64(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::U32(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::U64(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::I8(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::I16(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::U8(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::U16(i) => { let _ = write!(&mut handle, "{}", i); },
            VMValue::Bool(b) => { let _ = write!(&mut handle, "{}", b); },
            VMValue::F32(f) => { let _ = write!(&mut handle, "{}", f); },
            VMValue::F64(f) => { let _ = write!(&mut handle, "{}", f); },
            VMValue::Char(c) => { let _ = write!(&mut handle, "{}", c); },
            VMValue::Void => { let _ = write!(&mut handle, "void"); },
            _ => { let _ = write!(&mut handle, "[complex type]"); },
        };
    }
    
    // Add newline and flush
    let _ = writeln!(&mut handle);
    let _ = handle.flush();
    
    VMValue::Void
}
#[unsafe(no_mangle)]
pub extern "C" fn println_int(i: i32) {
    println!("{}", i);
}

#[unsafe(no_mangle)]
pub extern "C" fn println_bool(i: bool) {
    println!("{}", i);
}