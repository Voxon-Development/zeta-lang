use cranelift::prelude::{types, AbiParam};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use pest::error::LineColLocation::{Pos, Span};
use crate::ast::{ClassDecl, FuncDecl, Stmt};
use crate::compiler::Codegen;

mod parser;
mod ast;
mod compiler;

/*fn main() {
    match parser::parse_program("\
    fun main() {
        let hi = 1
    }
    ") {
        Ok(nodes) => {
            println!("Parsed successfully: {:?}", nodes);
            let compiler = Codegen::new()
        },
        Err(err) => {
            eprintln!("Parsing failed: {:?}", err);
            match err.line_col {
                Pos(pos) => eprintln!("At line: {}, column: {}", pos.0, pos.1),
                Span(span, ..) => eprintln!("At span: {:?}", span),
            }
        }
    }
}
*/

extern "C" fn println_str(s: *const u8) {
    unsafe {
        let cstr = std::ffi::CStr::from_ptr(s as *const i8);
        println!("{}", cstr.to_str().unwrap());
    }
}
extern "C" fn println_int(i: i32) {
    println!("{}", i);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let contents_of_file = std::fs::read_to_string("input.zeta")?;

    let stmts = parser::parse_program(contents_of_file.as_str())?; // Returns Vec<Stmt>
    
    let classes: Vec<ClassDecl> = stmts.iter()
        .filter(|stmt| if let Stmt::ClassDecl(c) = stmt { true } else { false })
        .map(|stmt| match stmt {
            Stmt::ClassDecl(c) => c.clone(),
            _ => panic!("Expected ClassDecl"),
        })
        .collect();

    for stmt in &stmts {
        println!("{:?}", stmt);
    }

    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names()).unwrap();

    builder.symbol("println_str", println_str as *const u8);
    builder.symbol("println_int", println_int as *const u8);
    let mut module = JITModule::new(builder);
    let mut codegen = Codegen::new();

    // Extract all function declarations
    let funcs: Vec<FuncDecl> = stmts.iter()
        .filter_map(|stmt| if let Stmt::FuncDecl(f) = stmt { Some(f.clone()) } else { None })
        .collect();

    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64)); // string pointer

    let mut another_sig = module.make_signature();
    another_sig.params.push(AbiParam::new(types::I32)); // string pointer
    // Return type if needed: sig.returns.push(...)

    let func_id = module.declare_function("println_str", Linkage::Import, &sig)?;
    codegen.func_ids.insert("println_str".to_string(), func_id);

    let func_id_2 = module.declare_function("println_int", Linkage::Import, &another_sig)?;
    codegen.func_ids.insert("println_int".to_string(), func_id_2);
    
    codegen.declare_classes(classes.as_slice(), &mut module)?;

    // --- First Pass: Declare all function signatures ---
    codegen.declare_funcs(&funcs, &mut module)?;

    // --- Second Pass: Define function bodies ---
    codegen.define_funcs(&funcs, &mut module)?;

    // --- Finalize all definitions ---
    module.finalize_definitions()?;

    // --- Find and call `main` ---
    if let Some(main_id) = codegen.func_ids.get("main") {
        let ptr = module.get_finalized_function(*main_id);
        let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(ptr) };
        main();
        Ok(())
    } else {
        println!("No `main` function found");
        Ok(())
    }
}
