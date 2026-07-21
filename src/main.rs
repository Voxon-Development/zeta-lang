use std::path::PathBuf;
use std::time::Instant;

use clap::{ArgMatches, CommandFactory, Error, FromArgMatches, Parser, Subcommand};
use snmalloc_rs::SnMalloc;
use zeta_compiler_api::Compiler;

use zeta_compiler_api::file_handling::compiler_lib_path;
use zeta_compiler_api::file_loader::choose_file_loader;
use zeta_compiler_api::main_structs::CompilerError;

#[global_allocator]
static ALLOCATOR: SnMalloc = SnMalloc;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[arg(short, long, global = true)]
    verbose: bool,

    #[arg(long, global = true, default_value = "true")]
    link_libc: bool,

    #[arg(long, global = true)]
    lib: Option<PathBuf>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build Zeta source files
    Build {
        path: Option<PathBuf>,

        #[arg(long, default_value_os = "./build")]
        out_dir: PathBuf,

        #[arg(short, long)]
        optimize: bool,

        #[arg(long)]
        emit_obj: bool,
    },

    /// Run Zeta source files directly
    Run {
        path: Option<PathBuf>,

        #[arg(last = true)]
        args: Vec<String>,
    },
}

fn main() -> Result<(), CompilerError<'static>> {
    let start = Instant::now();
    let mut matches: ArgMatches = Cli::command().get_matches();
    let cli_result: Result<Cli, Error> =
        Cli::from_arg_matches_mut(&mut matches).map_err(|err: clap::error::Error| {
            let mut cmd = Cli::command();
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

    let verbose = cli.verbose;

    let lib_override = cli.lib.clone();

    let result = match cli.command {
        Commands::Build {
            path,
            out_dir,
            optimize,
            emit_obj,
        } => {
            let source_path = path.clone().unwrap_or_else(|| PathBuf::from("./src"));
            if verbose {
                println!("Building from: {}", source_path.display());
                println!("Output directory: {}", out_dir.display());
                println!("Optimization: {}", optimize);
            }
            run_compiler(path, out_dir, optimize, verbose, emit_obj, lib_override)
        }
        Commands::Run { path, args } => {
            let source_path = path.clone().unwrap_or_else(|| PathBuf::from("./src"));
            if verbose {
                println!("Running from: {}", source_path.display());
                if !args.is_empty() {
                    println!("Arguments: {:?}", args);
                }
            }
            // TODO: implement run
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

fn run_compiler<'a, 'bump>(
    path: Option<PathBuf>,
    out_dir: PathBuf,
    optimize: bool,
    verbose: bool,
    emit_obj: bool,
    lib_override: Option<PathBuf>,
) -> Result<(), CompilerError<'a>>
where
    'bump: 'a,
    'a: 'bump,
{
    let stdlib_path = if let Some(custom) = lib_override {
        custom
    } else {
        compiler_lib_path()?
    };
    let source_path = path.unwrap_or_else(|| PathBuf::from("./src"));

    let mut compiler = Compiler::new()?;

    let file_loader = choose_file_loader();
    let stdlib_diags = compiler.load_directory(&file_loader, &stdlib_path, true)?;
    if stdlib_diags.has_errors() {
        stdlib_diags.report_all();
        return Err(CompilerError::ParserError(vec![]));
    }

    let user_diags = compiler.load_directory(&file_loader, &source_path, false)?;
    if user_diags.has_errors() {
        user_diags.report_all();
        return Err(CompilerError::TypeCheckError);
    }

    compiler.emit(&out_dir, optimize, verbose, emit_obj)?;
    Ok(())
}
