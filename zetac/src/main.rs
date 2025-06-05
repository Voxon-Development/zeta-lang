use crate::ast::{ClassDecl, FuncDecl, Stmt};
use crate::compiler::Codegen;
use cranelift::prelude::{types, AbiParam};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::println::*;
use clap::{Parser, Subcommand};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::process;

mod parser;
mod ast;
mod compiler;
mod println;
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

#[derive(Parser)]
#[command(name = "zeta")]
#[command(about = "The New Generation Programming Language: Zeta")]
#[command(version = "0.1.0")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Input file to run
    #[arg(value_name = "FILE")]
    file: Option<PathBuf>,

    /// Show verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    Jit {
        /// Input file
        file: PathBuf,
        /// Show verbose output
        #[arg(short, long)]
        verbose: bool,
    },
    Aot {
        file: PathBuf,

        #[arg(short, long)]
        output: Option<PathBuf>
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Jit { file, verbose }) => {
            Ok(run_file(file, false, verbose, None)?)
        },
        Some(Commands::Aot { file, output }) => {
            Ok(run_file(file, true, false, output)?)
        },
        None => {
            if let Some(file) = cli.file {
                Ok(run_file(file, false, cli.verbose, None)?)
            } else {
                eprintln!("No file specified. Use --help for usage information.");
                process::exit(1);
            }
        }
    }
}

fn run_file(file: PathBuf, native: bool, verbose: bool, native_output: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let instant = Instant::now();

    let module = compile_to_ir(file, native, native_output);

    match module {
        BackendModule::JIT(module, codegen) => {
            if let Some(main_id) = codegen.func_ids.get("main") {
                let ptr = module.get_finalized_function(*main_id);
                let main: extern "C" fn() -> i64 = unsafe { std::mem::transmute(ptr) };

                let other_instant = Instant::now();

                main();
                println!("Finished execution in {}ns", other_instant.elapsed().as_nanos());
                println!("Finished execution and compilation in {}ms", instant.elapsed().as_millis());

                Ok(())
            } else {
                eprintln!("No `main` function found");
                Ok(())
            }
        },
        BackendModule::Native(module, output, _codegen) => {
            let obj_bytes = module.finish().emit().expect("Should have been able to emit object file");

            let final_output = output.unwrap_or(PathBuf::from(Path::new("output.o")));
            std::fs::write(final_output.clone(), &obj_bytes).expect("Unable to write object file");
            println!("Wrote object file to {}", final_output.to_str().unwrap());
            Ok(())
        }
    }

    // --- Find and call `main` ---
}

fn compile_to_ir(file: PathBuf, native: bool, native_output: Option<PathBuf>) -> BackendModule {
    let contents_of_file = std::fs::read_to_string(file).expect("Unable to read file");

    let stmts = parser::parse_program(contents_of_file.as_str()).expect("Unable to parse program."); // Returns Vec<Stmt>

    let classes: Vec<ClassDecl> = stmts.iter()
        .filter_map(|stmt| if let Stmt::ClassDecl(s) = stmt { Some(s.clone()) } else { None })
        .collect();

    let unfinished_funcs: Vec<FuncDecl> = stmts.iter()
        .filter_map(|stmt| if let Stmt::FuncDecl(f) = stmt { Some(f.clone()) } else { None })
        .collect();

    let main_func: FuncDecl = unfinished_funcs.iter()
        .find_map(|stmt| if stmt.name != "main" { None } else { Some(stmt.clone()) })
        .expect("No `main` function found");

    let funcs = unfinished_funcs.iter()
        .filter_map(|f| if f.name != "main" { Some(f.clone()) } else { None })
        .collect::<Vec<FuncDecl>>();

    let mut codegen = Codegen::new();

    if native {
        // Native backend
        let isa = cranelift_native::builder().expect("Unable to build native ISA")
            .finish(cranelift::codegen::settings::Flags::new(cranelift::codegen::settings::builder()))
            .expect("Unable to create native ISA");
        let builder = ObjectBuilder::new(
            isa,
            "my_module".to_string(),
            cranelift_module::default_libcall_names(),
        ).expect("Unable to create object builder");
        let mut module = ObjectModule::new(builder);

        let mut sig = module.make_signature();
        let mut bool_sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64)); // string pointer
        bool_sig.params.push(AbiParam::new(types::I8)); // string pointer
        // Return type if needed: sig.returns.push(...)

        let func_id = module.declare_function("println_str", Linkage::Import, &sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_str".to_string(), func_id);

        let func_id_2 = module.declare_function("println_int", Linkage::Import, &sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_int".to_string(), func_id_2);

        let func_id_3 = module.declare_function("println_bool", Linkage::Import, &bool_sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_bool".to_string(), func_id_3);

        codegen.declare_classes(&classes, &mut module).expect("Unable to declare classes");
        
        let main_func_slice = &[main_func];
        codegen.declare_funcs_with_ret_type(main_func_slice, types::I32, &mut module).expect("Unable to declare functions");
        codegen.define_funcs(main_func_slice, &mut module).expect("Unable to define functions");

        codegen.declare_funcs(&funcs, &mut module).expect("Unable to declare functions");
        codegen.define_funcs(&funcs, &mut module).expect("Unable to define functions");

        BackendModule::Native(module, native_output, codegen)
    } else {
        // JIT backend
        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names()).expect("Unable to create JIT builder");
        builder.symbol("println_str", println_str as *const u8);
        builder.symbol("println_int", println_int as *const u8);
        builder.symbol("println_bool", println_bool as *const u8);
        let mut module = JITModule::new(builder);

        let mut sig = module.make_signature();
        let mut bool_sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64)); // string pointer
        bool_sig.params.push(AbiParam::new(types::I8)); // string pointer
        // Return type if needed: sig.returns.push(...)

        let func_id = module.declare_function("println_str", Linkage::Import, &sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_str".to_string(), func_id);

        let func_id_2 = module.declare_function("println_int", Linkage::Import, &sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_int".to_string(), func_id_2);

        let func_id_3 = module.declare_function("println_bool", Linkage::Import, &bool_sig).expect("Unable to define native method.");
        codegen.func_ids.insert("println_bool".to_string(), func_id_3);

        codegen.declare_classes(&classes, &mut module).expect("Unable to declare classes");
        codegen.declare_funcs(&unfinished_funcs, &mut module).expect("Unable to declare functions");
        codegen.define_funcs(&unfinished_funcs, &mut module).expect("Unable to define functions");
        module.finalize_definitions().expect("Unable to finalize definitions");

        BackendModule::JIT(module, codegen)
    }
}

enum BackendModule {
    JIT(JITModule, Codegen),
    Native(ObjectModule, Option<PathBuf>, Codegen),
}

