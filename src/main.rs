#![feature(allocator_api)]

mod compilation_passes;
mod file_handling;
mod link;
mod main_structs;

use engraver_assembly_emit::backend::Backend;
use engraver_assembly_emit::cranelift::cranelift_backend::CraneliftBackend;
use sentinel_typechecker::TypeChecker;
use zetaruntime::bump::GrowableBump;
use zetaruntime::string_pool::StringPool;

use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;
use std::time::Instant;

use clap::{ArgMatches, CommandFactory, Error, FromArgMatches, Parser, Subcommand};
use ir::hir::StrId;
use snmalloc_rs::SnMalloc;

use crate::compilation_passes::{check_all_module_bodies, register_all_modules};
use crate::file_handling::{
    ast_modules_from_parsed, collect_extern_c_names, collect_zeta_files, compiler_lib_path,
    emit_all, parse_files,
};
use crate::link::link;
use crate::main_structs::CompilerError;
use ir::errors::reporter::ErrorReporter;

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

    let stdlib_files = collect_zeta_files(&stdlib_path)?;
    if verbose {
        println!(
            "Compiling standard library ({} modules):",
            stdlib_files.len()
        );
        for f in &stdlib_files {
            println!("  [STD] {}", f.display());
        }
    }

    let source_path = path.unwrap_or_else(|| PathBuf::from("./src"));
    let user_files = collect_zeta_files(&source_path)?;
    if verbose {
        println!("Compiling project ({} modules):", user_files.len());
        for f in &user_files {
            println!("  [SRC] {}", f.display());
        }
    }

    let pool = Arc::new(StringPool::new().map_err(|_| CompilerError::FailedToAllocateStringPool)?);

    let stdlib_parsed = parse_files(stdlib_files, pool.clone())?;
    let user_parsed = parse_files(user_files, pool.clone())?;

    let mut error_reporter = ErrorReporter::new();
    for m in stdlib_parsed.iter().chain(user_parsed.iter()) {
        if m.parser_diagnostics.has_errors() {
            for error in &m.parser_diagnostics.errors {
                error_reporter
                    .add_parser_error(StrId::from(pool.intern(error.to_string().as_str())), None);
            }
        }
    }
    if error_reporter.has_errors() {
        error_reporter.report_all();
        return Err(CompilerError::ParserError(vec![]));
    }

    let stdlib_ast = ast_modules_from_parsed(&stdlib_parsed);
    let user_ast = ast_modules_from_parsed(&user_parsed);

    let all_ast: Vec<_> = stdlib_ast.iter().chain(user_ast.iter()).copied().collect();
    let stdlib_len = stdlib_ast.len();
    let total_len = all_ast.len();

    let mut dep_graph = codex_dependency_graph::dep_graph::DepGraph::new();
    dep_graph.build_from_ast(&all_ast, &pool);

    let user_indices: Vec<usize> = (stdlib_len..total_len).collect();
    for stdlib_idx in 0..stdlib_len {
        dep_graph.link_stdlib_to_user(stdlib_idx, &user_indices);
    }

    // Surface unresolved imports as warnings for now
    for unresolved in &dep_graph.unresolved_imports {
        eprintln!(
            "Warning: unresolved import in module {}: {:?}",
            unresolved.from_module_idx, unresolved.path
        );
    }

    let all_parsed: Vec<&main_structs::ModuleWithArena<'a, 'bump>> =
        stdlib_parsed.iter().chain(user_parsed.iter()).collect();

    let lowerer_bump = GrowableBump::new(4096, 8);
    let (hir_modules, errors) =
        lower_all_from_refs(&all_parsed, &dep_graph, &lowerer_bump, pool.clone());

    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error)
        }

        return Ok(());
    }

    let compilation_order = dep_graph.get_module_compilation_order();

    let mut backend = CraneliftBackend::new(pool.clone(), optimize, verbose);
    let extern_c_names = collect_extern_c_names(&hir_modules);
    emit_all(
        &hir_modules,
        &compilation_order,
        &mut backend,
        pool.clone(),
        &extern_c_names,
        &dep_graph,
    );

    let out_obj = match backend
        .finish(&out_dir)
        .map_err(|e| CompilerError::FinishError(Box::new(e)))
    {
        Ok(out_obj) => out_obj,
        Err(e) => return Err(e),
    };

    if !emit_obj {
        let program_path = out_dir.join("program");
        link(
            &[out_obj.to_str().unwrap()],
            program_path.to_str().unwrap(),
            true,
        )?;
    }

    Ok(())
}

fn lower_all_from_refs<'a, 'bump>(
    modules: &[&'bump crate::main_structs::ModuleWithArena<'a, 'bump>],
    dep_graph: &'a codex_dependency_graph::dep_graph::DepGraph,
    bump: &'bump GrowableBump<'bump>,
    pool: Arc<StringPool>,
) -> (Vec<ir::hir::HirModule<'a, 'bump>>, Vec<CompilerError<'a>>) {
    use crate::compilation_passes::pass_hir_lowering;

    let mut errors: Vec<CompilerError<'a>> = Vec::new();
    let type_checker = Rc::new(RefCell::new(TypeChecker::new(dep_graph, bump)));

    let mut file_names_and_contents: Vec<(StrId, StrId)> = Vec::with_capacity(modules.len());
    let lowered_modules = modules
        .iter()
        .enumerate()
        .filter_map(|(module_idx, m)| {
            let mut stmt_vec = Vec::new_in(bump);
            for stmt in m.stmts {
                stmt_vec.push(*stmt);
            }

            file_names_and_contents.push((m.name, m.source));

            match pass_hir_lowering(
                stmt_vec,
                pool.clone(),
                m.bump.clone(),
                dep_graph,
                module_idx,
            ) {
                Ok(hir) => Some(hir),
                Err(e) => {
                    eprintln!("Compilation failed for module {:?}: {}", m.name, e);
                    errors.push(e);
                    None
                }
            }
        })
        .collect::<Vec<_>>();

    register_all_modules(lowered_modules.as_slice(), type_checker.clone());

    if let Err(e) = check_all_module_bodies(
        lowered_modules.as_slice(),
        file_names_and_contents.as_slice(),
        type_checker,
    ) {
        errors.push(e);
    }
    (lowered_modules, errors)
}
