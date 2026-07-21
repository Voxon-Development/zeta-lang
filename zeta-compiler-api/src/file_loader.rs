use std::path::PathBuf;

use crate::io_uring_file_loader::IoUringFileLoader;

pub trait FileLoader {
    fn load_files(&self, paths: &[PathBuf]) -> Result<Vec<SourceFile>, std::io::Error>;
}

#[derive(Clone)]
pub struct SourceFile {
    pub path: PathBuf,
    pub source: String,
}

pub fn choose_file_loader() -> impl FileLoader {
    IoUringFileLoader::new(256)
}
