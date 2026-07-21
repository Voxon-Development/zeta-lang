use std::{env::home_dir, path::PathBuf, time::Instant};

use clap::{Parser, Subcommand};

const GITHUB_REPO: &str = "Voxon-Development/zeta-lang";
const TOOLCHAIN_ASSET_NAME: &str = "zeta-linux-x86_64.tar.gz";
const CURRENT_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Parser)]
#[command(name = "zetaup", version, about = "Zeta toolchain updater")]
struct Cli {
    #[arg(short, long)]
    verbose: bool,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Upgrade Zeta compiler, LSP, and standard library
    Upgrade,
}

fn main() {
    let start = Instant::now();
    let cli = Cli::parse();

    match cli.command {
        Commands::Upgrade => {
            if let Err(e) = run_upgrade(cli.verbose) {
                eprintln!("Upgrade failed: {e}");
                let duration = start.elapsed();
                println!("Operation completed in {:.2?}", duration);
                std::process::exit(1);
            }
        }
    }

    let duration = start.elapsed();
    println!("Operation completed in {:.2?}", duration);
}

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

    let release: GhRelease = ureq::get(&url)
        .header("User-Agent", "zetaup")
        .call()?
        .body_mut()
        .read_json::<GhRelease>()?;

    let latest_version = release.tag_name.trim_start_matches('v');

    if !is_newer(latest_version, CURRENT_VERSION) {
        println!("Already up to date (v{CURRENT_VERSION}).");
        return Ok(());
    }

    let asset = release
        .assets
        .iter()
        .find(|a| a.name == TOOLCHAIN_ASSET_NAME)
        .ok_or("release missing linux x86_64 toolchain")?;

    let temp = std::env::temp_dir().join("zeta-toolchain");

    if temp.exists() {
        std::fs::remove_dir_all(&temp)?;
    }

    std::fs::create_dir_all(&temp)?;

    download_and_extract(&asset.browser_download_url, &temp, verbose)?;

    let bin_dir = home_dir().ok_or("no home directory")?.join(".local/bin");

    let data_dir = data_local_dir()
        .ok_or("no local data directory")?
        .join("zeta");

    std::fs::create_dir_all(&bin_dir)?;
    std::fs::create_dir_all(&data_dir)?;

    install_binary(&temp.join("zeta-lang"), &bin_dir.join("zeta-lang"))?;

    install_binary(&temp.join("zeta-lsp"), &bin_dir.join("zeta-lsp"))?;

    // This will have to be done manually by the User if there is any updates to zetaup, lol
    //install_binary(&temp.join("zetaup"), &bin_dir.join("zetaup"))?;

    if data_dir.join("lib").exists() {
        std::fs::remove_dir_all(data_dir.join("lib"))?;
    }

    std::fs::rename(temp.join("lib"), data_dir.join("lib"))?;

    println!("Upgraded Zeta to v{latest_version}");

    Ok(())
}

fn download_and_extract(
    url: &str,
    destination: &PathBuf,
    verbose: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if verbose {
        println!("Downloading {url}");
    }

    let response = ureq::get(url).header("User-Agent", "zetaup").call()?;

    let decoder = flate2::read::GzDecoder::new(response.into_body().into_reader());

    let mut archive = tar::Archive::new(decoder);

    archive.unpack(destination)?;

    Ok(())
}

fn install_binary(
    source: &PathBuf,
    destination: &PathBuf,
) -> Result<(), Box<dyn std::error::Error>> {
    std::fs::copy(source, destination)?;

    use std::os::unix::fs::PermissionsExt;

    let mut perms = std::fs::metadata(destination)?.permissions();

    perms.set_mode(0o755);

    std::fs::set_permissions(destination, perms)?;

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

fn data_local_dir() -> Option<PathBuf> {
    #[cfg(target_os = "linux")]
    {
        std::env::var_os("HOME")
            .map(PathBuf::from)
            .map(|home| home.join(".local"))
    }

    #[cfg(not(target_os = "linux"))]
    {
        panic!("data_local_dir is only supported on Linux");
    }
}
