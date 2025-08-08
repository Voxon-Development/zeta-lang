use crate::backend::compiler::Backend;
use crate::backend::cranelift_backend::CraneliftBackend;
use crate::frontend::ast::{ClassDecl, Stmt};
use crate::frontend::hir_lowerer::HirLowerer;
use crate::midend::ir::lowerer::MirLowerer;
use crate::midend::type_checker::rules::type_inference::VarInferenceRule;
use crate::midend::type_checker::type_checker::TypeChecker;

pub mod backend;
pub mod frontend;
pub mod midend;

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

/*fn main() -> Result<(), Box<dyn std::error::Error>> {
    let instant = Instant::now();

    let contents_of_file = std::fs::read_to_string("input.zeta")?;

    let stmts = frontend::parser::parse_program(contents_of_file.as_str())?; // Returns Vec<Stmt>
    
    let classes: Vec<ClassDecl> = stmts.iter()
        .filter_map(|stmt| if let Stmt::ClassDecl(_) = stmt { Some(get_stmt_as_class(&stmt)) } else { None })
        .collect();

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
    // Return type if needed: sig.returns.push(...)

    let func_id = module.declare_function("println_str", Linkage::Import, &sig)?;
    codegen.func_ids.insert("println_str".to_string(), func_id);

    let func_id_2 = module.declare_function("println_int", Linkage::Import, &sig)?;
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

        // main function from zeta executed

        let other_instant = Instant::now();
        main();
        println!("Finished in {}ns", other_instant.elapsed().as_nanos());

        println!("Finished in {}ms", instant.elapsed().as_millis());

        Ok(())
    } else {
        println!("No `main` function found");
        Ok(())
    }
}*/

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Get the contents of the file
    // AST = Abstract Syntax Tree
    // HIR = High-Level Intermediate Representation
    // MIR = Mid-Level Intermediate Representation
    let contents_of_file = std::fs::read_to_string("input.zeta")?;

    // Parse the contents of the file to a Vec<Stmt> (AST)
    let stmts = frontend::parser::parse_program(contents_of_file.as_str())?;

    // Lower AST to HIR
    let mut lowerer = HirLowerer::new();
    let hir_module = lowerer.lower_module(stmts);

    // Check HIR for errors of any kind
    let mut type_checker = TypeChecker::new(&lowerer.ctx);
    type_checker.add_rule(VarInferenceRule);
    type_checker.check_module(&hir_module);

    // Lower HIR to MIR
    let mir_ctx = MirLowerer::new();
    let mir_module = mir_ctx.lower_module(&hir_module);

    // Compile MIR to machine code using cranelift
    let mut backend = CraneliftBackend::new();
    backend.emit_module(&mir_module);

    // --- Finalize all definitions and write to out.o ---
    backend.finish();

    Ok(())
}

#[inline]
fn get_stmt_as_class(stmt: &Stmt) -> ClassDecl {
    match stmt {
        Stmt::ClassDecl(c) => c.clone(),
        _ => panic!("Expected ClassDecl"),
    }
}
