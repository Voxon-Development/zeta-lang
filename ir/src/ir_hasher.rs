use std::hash::BuildHasher;
use fxhash::FxHashMap;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct FxHashBuilder;

pub type HashMap<K, V> = FxHashMap<K, V>;

impl BuildHasher for FxHashBuilder {
    type Hasher = fxhash::FxHasher;
    fn build_hasher(&self) -> Self::Hasher {
        fxhash::FxHasher::default()
    }
}