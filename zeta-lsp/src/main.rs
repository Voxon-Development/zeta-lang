pub mod dispatch;
pub mod docs;
pub mod lsp_diagnostics;
pub mod semantic_tokens;
pub mod server;
pub mod state;
pub mod text_utils;
pub mod urls;

use std::error::Error;

use clap::{Parser, Subcommand};
pub use server::Server;

#[derive(Parser)]
#[command(
    name = "zeta-lsp",
    author,
    version,
    about = "Zeta Language Server",
    long_about = None
)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Start the language server
    Lsp,

    /// Index documentation for Zed
    IndexDocs,

    /// Print version information
    Version,

    /// Type-check a project
    Check {
        /// Project directory
        path: Option<std::path::PathBuf>,
    },

    /// Format source files
    Format {
        /// File or directory
        path: std::path::PathBuf,
    },
}

fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match cli.command.unwrap_or(Commands::Lsp) {
        Commands::Lsp => {
            let compiler = zeta_compiler_api::Compiler::new().map_err(|e| Box::new(e))?;
            let server = Server::new(compiler)?;
            server.run()?;
        }

        Commands::IndexDocs => {
            docs::index()?;
        }

        Commands::Version => {
            println!("Zeta version {}", env!("CARGO_PKG_VERSION"));
        }

        Commands::Check { path } => {
            println!("Checking {:?}", path);
            // TODO
        }

        Commands::Format { path } => {
            println!("Formatting {:?}", path);
            // TODO
        }
    }

    Ok(())
}
