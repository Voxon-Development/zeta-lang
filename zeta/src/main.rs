mod vm;

use crate::vm::virtualmachine::VirtualMachine;
use clap::{Parser, Subcommand};
use std::error::Error;
use std::path::{Path, PathBuf};
use std::time::Instant;
use std::{fs, process};
use parking_lot::Mutex;
use trc::SharedTrc;
use zetac::{compile_files_async, compile_to_ir, BackendModule};
use crate::vm::functions;

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

fn ensure_dir(path: &Path) -> std::io::Result<()> {
    if !path.exists() {
        fs::create_dir_all(path)?;
    }
    Ok(())
}

fn clean_target_dir(target_dir: &Path, verbose: bool) -> std::io::Result<()> {
    if target_dir.exists() {
        if verbose {
            println!("Cleaning target directory: {}", target_dir.display());
        }
        std::fs::remove_dir_all(target_dir)?;
    }
    Ok(())
}

async fn build_project(verbose: bool) -> Result<(), Box<dyn Error>> {
    let src_dir = Path::new("src");
    let target_dir = Path::new("target");

    if !src_dir.exists() {
        return Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "src directory not found",
        )));
    }

    // Ensure target directory exists
    ensure_dir(target_dir)?;

    // Find all .zeta files in src directory
    let mut files = Vec::new();
    for entry in walkdir::WalkDir::new(src_dir)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|e| e.file_type().is_file())
    {
        if let Some(ext) = entry.path().extension() {
            if ext == "zeta" {
                files.push(entry.path().to_path_buf());
            }
        }
    }

    if files.is_empty() {
        return Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "No .zeta files found in src directory",
        )));
    }

    if verbose {
        println!("Found {} source files", files.len());
    }

    // Compile all files
    let (module, class_table) = compile_files_async(files.clone())
        .await
        .map_err(|e| {
            eprintln!("Compilation failed: {}", e);
            std::process::exit(1);
        })?;

    // Save the compiled module to target/
    let output_path = target_dir.join("module.zb");

    if verbose {
        println!("Compilation successful. Output: {}", output_path.display());
    }

    Ok(())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();
    
    match cli.command {
        Some(Commands::Jit { file, verbose }) => {
            run_files_async(vec![file], true).await?;
        },
        Some(Commands::Aot { file, output }) => {
            run_file(file, true, false, output)?;
        },
        None => {
            if let Some(file) = cli.file {
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

    let (module, class_table) = compile_files_async(files.clone())
        .await
        .map_err(|e| {
            eprintln!("Async compilation failed: {}", e);
            process::exit(1);
        })?;

    let duration = start_time.elapsed();

    if verbose {
        println!("Compilation completed in {:.2?}", duration);
        println!("Module contains {} functions", module.functions.len());
    }

    let vm = VirtualMachine::new(module, class_table.compressed);
    let vm = boot(vm);

    let maybe_main = vm.lock().function_module.lock().entry;
    if let Some(main_id) = maybe_main {
        unsafe { functions::run_native_ptr(main_id, std::ptr::null(), 0); }
        vm.lock().shutdown();
    } else {
        eprintln!("No entry point found. Expected: `void main() {{ println(\"Hello World!\"); }}`");
        vm.lock().shutdown();
    }

    Ok(())
}

pub fn boot(vm: VirtualMachine) -> SharedTrc<Mutex<VirtualMachine>> {
    let shared = SharedTrc::new(Mutex::new(vm));
    //shared.lock().event_loop.run(shared.clone());

    shared
}


pub fn run_file(file: PathBuf, native: bool, verbose: bool, native_output: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let instant = Instant::now();

    let module = compile_to_ir(file, false, native_output);

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