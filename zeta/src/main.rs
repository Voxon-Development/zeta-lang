mod vm;

use std::error::Error;
use std::path::{Path, PathBuf};
use std::time::Instant;
use clap::{Parser, Subcommand};
use zetac::{compile_to_ir, BackendModule, Codegen};

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
            run_file(file, false, verbose, None)?;
        },
        Some(Commands::Aot { file, output }) => {
            run_file(file, true, false, output)?;
        },
        None => {
            if let Some(file) = cli.file {
                run_file(file, false, cli.verbose, None)?;
            } else {
                eprintln!("No file specified. Use --help for usage information.");
                std::process::exit(1);
            }
        }
    }
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