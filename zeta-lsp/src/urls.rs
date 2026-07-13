use anyhow::{Context, Result};
use std::path::{Path, PathBuf};

pub fn uri_to_path(uri: &lsp_types::Uri) -> Result<PathBuf> {
    let url =
        url::Url::parse(uri.as_str()).with_context(|| format!("invalid URI: {}", uri.as_str()))?;
    url.to_file_path()
        .map_err(|_| anyhow::anyhow!("not a file:// URI: {}", uri.as_str()))
}

pub fn path_to_uri(path: &Path) -> Result<lsp_types::Uri> {
    let url = url::Url::from_file_path(path)
        .map_err(|_| anyhow::anyhow!("path is not absolute: {}", path.display()))?;
    url.as_str()
        .parse::<lsp_types::Uri>()
        .with_context(|| format!("failed to build Uri from {}", url))
}
