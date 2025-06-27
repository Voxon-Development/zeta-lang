use ir::VMValue;

#[unsafe(no_mangle)]
pub fn println(args: &[VMValue]) {
    let s = &args[0];
    match s {
        VMValue::Str(s) => {
            println!("{}", s);
        },
        VMValue::I32(i) => {
            println!("{}", i);
        },
        VMValue::I64(i) => {
            println!("{}", i);
        },
        VMValue::U32(i) => {
            println!("{}", i);
        },
        VMValue::U64(i) => {
            println!("{}", i);
        },
        VMValue::I8(i) => {
            println!("{}", i);
        },
        VMValue::I16(i) => {
            println!("{}", i);
        },
        VMValue::U8(i) => {
            println!("{}", i);
        },
        VMValue::U16(i) => {
            println!("{}", i);
        },
        VMValue::Ptr(i) => {
            println!("{}", i);
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