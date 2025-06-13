mod vm;

use std::error::Error;
use std::path::{Path, PathBuf};
use std::{fs, process};
use std::any::Any;
use std::time::Instant;
use clap::{Parser, Subcommand};
use zetac::{compile_to_ir, frontend, BackendModule, Codegen, compile_files_async};
use zetac::ast::{ClassDecl, FuncDecl, Stmt};
use zetac::codegen::ir::ir_compiler::IrCompiler;
use zetac::codegen::ir::module::ZetaModule;
use crate::vm::virtualmachine::VirtualMachine;
use zetac::AsyncCompileError;

#[derive(Parser)]
#[command(name = "zeta")]
#[command(about = "The New Generation Programming Language: Zeta")]
#[command(version = "0.1.0")]
pub struct Cli {
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

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::Jit { file, verbose }) => {
            run_files_async(vec![file], verbose).await?;
        },
        Some(Commands::Aot { file, output }) => {
            // For AOT compilation, we'll still use the old synchronous version
            // since we need to generate object files
            run_file(file, true, false, output)?;
        },
        None => {
            if let Some(file) = cli.file {
                // For single file, use the async version for consistency
                run_files_async(vec![file], cli.verbose).await?;
            } else {
                eprintln!("No file specified. Use --help for usage information.");
                std::process::exit(1);
            }
        }
    }
    
    Ok(())
}

async fn run_files_async(files: Vec<PathBuf>, verbose: bool) -> Result<(), Box<dyn Error>> {
    if verbose {
        println!("Compiling {} files asynchronously...", files.len());
    }
    
    let start_time = Instant::now();
    
    let module = compile_files_async(files.clone())
        .await
        .map_err(|e| {
            eprintln!("Async compilation failed: {}", e);
            std::process::exit(1);
        })?;
    
    let duration = start_time.elapsed();
    
    if verbose {
        println!("Compilation completed in {:.2?}", duration);
        println!("Module contains {} functions", module.functions.len());
    }
    
    // Run the compiled code
    let mut vm = VirtualMachine::new(module);
    vm.run();
    
    Ok(())
}

pub fn run_file(file: PathBuf, native: bool, verbose: bool, native_output: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
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