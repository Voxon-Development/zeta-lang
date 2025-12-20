#![feature(str_as_str)]
#![feature(allocator_api)]

mod link;
mod main_structs;
mod file_handling;
mod compilation_passes;

use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use ir::hir::HirModule;
use zetaruntime::string_pool::StringPool;

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Instant;

use clap::{ArgMatches, CommandFactory, Error, FromArgMatches, Parser, Subcommand};
use snmalloc_rs::SnMalloc;
use ir::hir::StrId;

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

use crate::file_handling::{collect_zeta_files, compile_files, compiler_lib_path, emit_all};
use crate::link::link;
use crate::main_structs::CompilerError;
use ir::errors::reporter::ErrorReporter;

/// Main CLI application
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    #[arg(long, global = true, default_value = "true")]
    link_libc: bool,

    #[arg(long, global = true)]
    lib: Option<PathBuf>,

    #[command(subcommand)]
    command: Commands,
}

/// Available subcommands
#[derive(Subcommand)]
enum Commands {
    /// Build Zeta source files
    Build {
        /// Path to the source file or directory (use -- for default ./src)
        path: Option<PathBuf>,
        
        /// Output directory for compiled artifacts
        #[arg(long, default_value_os = "./build")]
        out_dir: PathBuf,
        
        /// Optimize the generated code
        #[arg(short, long)]
        optimize: bool,

        /// Optimize the generated code
        #[arg(long)]
        emit_obj: bool,
    },
    
    /// Run Zeta source files directly
    Run {
        /// Path to the source file or directory (use -- for default ./src)
        path: Option<PathBuf>,
        
        /// Arguments to pass to the program
        #[arg(last = true)]
        args: Vec<String>,
    },
}

// entry point
fn main() -> Result<(), CompilerError> {
    let start = Instant::now();
    let mut matches: ArgMatches = Cli::command().get_matches();
    let cli_result: Result<Cli, Error> = Cli::from_arg_matches_mut(&mut matches)
        .map_err(|err: clap::error::Error| {
            let mut cmd: clap::Command = Cli::command();
            err.format(&mut cmd)
        });

    let cli: Cli = match cli_result {
        Ok(cli) => cli,
        Err(error) => {
            error.print().expect("failed to print error message");
            let duration = start.elapsed();
            println!("Operation completed in {:.2?}", duration);
            std::process::exit(error.exit_code());
        }
    };
    
    let result: Result<(), CompilerError> = match &cli.command {
        Commands::Build { path, out_dir, optimize, emit_obj } => {
            let source_path: PathBuf = path.clone().unwrap_or_else(|| PathBuf::from("./src"));
            if cli.verbose {
                println!("Building from: {}", source_path.display());
                println!("Output directory: {}", out_dir.display());
                println!("Optimization: {}", optimize);
            }
            run_compiler(path.clone(), &out_dir, *optimize, cli.verbose, *emit_obj, cli.lib)
        }
        Commands::Run { path, args } => {
            let source_path: PathBuf = path.clone().unwrap_or_else(|| PathBuf::from("./src"));
            if cli.verbose {
                println!("Running from: {}", source_path.display());
                if !args.is_empty() {
                    println!("Arguments: {:?}", args);
                }
            }
            //run_compiler(path.clone(), &out_dir, *optimize, cli.verbose, *emit_obj, cli.lib);
            // TODO: Implement actual run logic
            Ok(())
        }
    };

    let duration = start.elapsed();
    println!("Operation completed in {:.2?}", duration);
    
    match result {
        Ok(_) => Ok(()),
        Err(e) => {
            eprintln!("Error: {}", e);
            Err(e)
        }
    }
}

fn run_compiler(
    path: Option<PathBuf>,
    out_dir: &PathBuf,
    optimize: bool,
    verbose: bool,
    emit_obj: bool,
    lib_override: Option<PathBuf>,
) -> Result<(), CompilerError> {
    let stdlib_path: PathBuf = if let Some(custom) = lib_override {
        custom
    } else {
        compiler_lib_path()?
    };

    let stdlib_files: Vec<PathBuf> = collect_zeta_files(&stdlib_path)?;

    if verbose {
        println!("Compiling standard library ({}) modules:", stdlib_files.len());
        for f in &stdlib_files {
            println!("  [STD] {}", f.display());
        }
    }

    let string_pool = StringPool::new()
        .map_err(|_| CompilerError::FailedToAllocateStringPool)?;
    let arc: Arc<StringPool> = Arc::new(string_pool);

    let stdlib_modules_result: Vec<_> = compile_files(&stdlib_files, arc.clone())?;
    
    // Collect parser diagnostics from stdlib
    let mut error_reporter = ErrorReporter::new();
    let mut stdlib_modules = Vec::new();
    for module_with_arena in stdlib_modules_result {
        if module_with_arena.parser_diagnostics.has_errors() {
            for error in &module_with_arena.parser_diagnostics.errors {
                error_reporter.add_parser_error(StrId::from(arc.intern(error.to_string().as_str())), None);
            }
        }
        if module_with_arena.valid {
            stdlib_modules.push(module_with_arena.module);
        }
    }

    let source_path: PathBuf = path.unwrap_or_else(|| PathBuf::from("./src"));
    let user_files: Vec<PathBuf> = collect_zeta_files(&source_path)?;

    if verbose {
        println!("Compiling project ({} modules):", user_files.len());
        for f in &user_files {
            println!("  [SRC] {}", f.display());
        }
    }

    let user_modules_result: Vec<_> = compile_files(&user_files, arc.clone())?;
    
    // Collect parser diagnostics from user code
    let mut user_modules = Vec::new();
    for module_with_arena in user_modules_result {
        if module_with_arena.parser_diagnostics.has_errors() {
            for error in &module_with_arena.parser_diagnostics.errors {
                error_reporter.add_parser_error(StrId::from(arc.intern(error.to_string().as_str())), None);
            }
        }
        if module_with_arena.valid {
            user_modules.push(module_with_arena.module);
        }
    }
    
    // Report all parser errors after HIR lowering
    if error_reporter.has_errors() {
        error_reporter.report_all();
        return Err(CompilerError::ParserError(vec![])); // Empty vec since errors already reported
    }

    let mut backend = CraneliftBackend::new(arc.clone(), optimize, verbose);

    emit_all(stdlib_modules, &mut backend, arc.clone());
    emit_all(user_modules, &mut backend, arc.clone());

    let out_obj: PathBuf = backend.finish(out_dir).map_err(CompilerError::FinishError)?;
    if !emit_obj {
        let program_path: PathBuf = out_dir.join("program");
        link(&[out_obj.to_str().unwrap()], program_path.to_str().unwrap(), true)?;
    }

    Ok(())
}