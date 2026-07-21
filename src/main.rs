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

const CURRENT_VERSION: &'static str = env!("CARGO_PKG_VERSION");

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
    Upgrade,
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

    if let Commands::Upgrade = cli.command {
        if let Err(e) = run_upgrade(verbose) {
            eprintln!("Upgrade failed: {e}");
            std::process::exit(1);
        }
        let duration = start.elapsed();
        println!("Operation completed in {:.2?}", duration);
        return Ok(());
    }

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
        Commands::Upgrade => unreachable!("handled above"),
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

const GITHUB_REPO: &str = "Voxon-Development/zeta-lang";
const COMPILER_ASSET_NAME: &str = "zeta-compiler-x86_64-unknown-linux-gnu";
const LSP_ASSET_NAME: &str = "zeta-lsp-x86_64-unknown-linux-gnu";

#[derive(serde::Deserialize)]
struct GhAsset {
    name: String,
    browser_download_url: String,
}

#[derive(serde::Deserialize)]
struct GhRelease {
    tag_name: String,
    assets: Vec<GhAsset>,
}

fn run_upgrade(verbose: bool) -> Result<(), Box<dyn std::error::Error>> {
    let url = format!("https://api.github.com/repos/{GITHUB_REPO}/releases/latest");
    // GitHub's API requires a User-Agent header on every request or it 403s.
    let release: GhRelease = ureq::get(&url)
        .header("User-Agent", "zeta-compiler-upgrade")
        .call()?
        .body_mut()
        .read_json::<GhRelease>()?;

    let latest_version = release.tag_name.trim_start_matches('v');

    if !is_newer(latest_version, CURRENT_VERSION) {
        println!("Already up to date (v{CURRENT_VERSION}).");
        return Ok(());
    }
    println!("New version available: v{latest_version} (current: v{CURRENT_VERSION})");

    let compiler_asset = release
        .assets
        .iter()
        .find(|a| a.name == COMPILER_ASSET_NAME)
        .ok_or(format!(
            "release v{latest_version} is missing asset `{COMPILER_ASSET_NAME}`"
        ))?;
    let lsp_asset = release.assets.iter().find(|a| a.name == LSP_ASSET_NAME);

    let current_exe = std::env::current_exe()?;
    let install_dir = current_exe
        .parent()
        .ok_or("running binary has no parent directory")?;

    download_and_replace(
        &compiler_asset.browser_download_url,
        &install_dir.join("zeta-compiler"),
        verbose,
    )?;

    match lsp_asset {
        Some(lsp_asset) => {
            download_and_replace(
                &lsp_asset.browser_download_url,
                &install_dir.join("zeta-lsp"),
                verbose,
            )?;
        }
        None if verbose => {
            println!(
                "Note: release v{latest_version} has no `{LSP_ASSET_NAME}` asset, skipping zeta-lsp."
            );
        }
        None => {}
    }

    println!("Upgraded to v{latest_version}.");
    Ok(())
}

fn download_and_replace(
    url: &str,
    dest: &PathBuf,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if verbose {
        println!("Downloading {url}");
    }

    let mut reader = ureq::get(url)
        .header("User-Agent", "zeta-compiler-upgrade")
        .call()?
        .into_body()
        .into_reader();

    let tmp_path = dest.with_extension("new");
    {
        let mut file = std::fs::File::create(&tmp_path)?;
        std::io::copy(&mut reader, &mut file)?;
    }

    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(&tmp_path)?.permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(&tmp_path, perms)?;

    std::fs::rename(&tmp_path, dest)?;

    if verbose {
        println!("Installed {}", dest.display());
    }
    Ok(())
}

fn parse_version(v: &str) -> (u64, u64, u64, Option<&str>) {
    let (core, pre) = v.split_once('-').map_or((v, None), |(c, p)| (c, Some(p))); // alpha/beta
    let mut parts = core.split('.').map(|p| p.parse::<u64>().unwrap_or(0));
    (
        parts.next().unwrap_or(0),
        parts.next().unwrap_or(0),
        parts.next().unwrap_or(0),
        pre,
    )
}

fn is_newer(latest: &str, current: &str) -> bool {
    let (lmaj, lmin, lpatch, lpre) = parse_version(latest);
    let (cmaj, cmin, cpatch, cpre) = parse_version(current);

    if (lmaj, lmin, lpatch) != (cmaj, cmin, cpatch) {
        return (lmaj, lmin, lpatch) > (cmaj, cmin, cpatch);
    }
    match (cpre, lpre) {
        (Some(_), None) => true,
        (None, Some(_)) => false,
        (Some(a), Some(b)) => a != b,
        (None, None) => false,
    }
}
