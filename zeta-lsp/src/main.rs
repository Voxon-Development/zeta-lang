pub mod dispatch;
pub mod lsp_diagnostics;
pub mod semantic_tokens;
pub mod server;
pub mod state;
pub mod text_utils;
pub mod urls;

pub use server::Server;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let compiler = zeta_compiler_api::Compiler::new().map_err(|e| Box::new(e))?;

    let server = Server::new(compiler)?;
    server.run()?;

    Ok(())
}
