use ir::ir_hasher::HashMap;
use lsp_types::Uri;
use std::path::PathBuf;
use zeta_compiler_api::Compiler;

pub struct Document {
    pub version: i32,
    pub path: PathBuf,
    pub text: String,
}

pub struct ServerState<'a, 'bump> {
    pub compiler: Compiler<'a, 'bump>,
    pub workspace_root: Option<PathBuf>,
    pub documents: HashMap<Uri, Document>,
    pub should_exit: bool,
}
