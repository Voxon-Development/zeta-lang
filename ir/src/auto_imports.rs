/// Package paths implicitly imported into every module, without an
/// explicit `import`. Plain string segments (not `StrId`) so this can be
/// constructed without a `StringPool`; paths are interned + resolved
/// against the `DepGraph` lazily, once per module registration.
#[derive(Debug, Clone)]
pub struct AutoImportRegistry {
    paths: Vec<Vec<&'static str>>,
}

pub const DEFAULT_AUTO_IMPORTS: &[&[&str]] = &[
    &["zeta", "lang"],
    &["zeta", "string"],
    &["zeta", "prelude"],
    &["zeta", "result"],
];

impl Default for AutoImportRegistry {
    fn default() -> Self {
        Self {
            paths: DEFAULT_AUTO_IMPORTS.iter().map(|p| p.to_vec()).collect(),
        }
    }
}

impl AutoImportRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn empty() -> Self {
        Self { paths: Vec::new() }
    }

    pub fn add(&mut self, path: &[&'static str]) {
        if !self.paths.iter().any(|p| p.as_slice() == path) {
            self.paths.push(path.to_vec());
        }
    }

    pub fn remove(&mut self, path: &[&str]) {
        self.paths.retain(|p| p.as_slice() != path);
    }

    pub fn paths(&self) -> impl Iterator<Item = &[&'static str]> {
        self.paths.iter().map(|p| p.as_slice())
    }
}
