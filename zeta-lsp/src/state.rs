use lsp_types::Uri;
use std::{collections::HashMap, path::PathBuf};
use zeta_compiler_api::Compiler;

pub struct Document {
    pub version: i32,
    pub path: PathBuf,
}

pub struct ServerState<'a, 'bump> {
    pub compiler: Compiler<'a, 'bump>,
    pub workspace_root: Option<PathBuf>,
    pub documents: HashMap<Uri, Document>,
    pub should_exit: bool,
}
