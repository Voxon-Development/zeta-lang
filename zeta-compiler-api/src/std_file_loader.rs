use std::path::PathBuf;

use crate::file_loader::{FileLoader, SourceFile};

pub struct StdFileLoader;

impl FileLoader for StdFileLoader {
    fn load_files(&self, paths: &[PathBuf]) -> Result<Vec<SourceFile>, std::io::Error> {
        paths
            .iter()
            .map(|path| {
                Ok(SourceFile {
                    path: path.clone(),
                    source: std::fs::read_to_string(path)?,
                })
            })
            .collect()
    }
}
