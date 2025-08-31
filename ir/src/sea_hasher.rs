use std::hash::BuildHasher;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct SeaHashBuilder;

impl BuildHasher for SeaHashBuilder {
    type Hasher = seahash::SeaHasher;
    fn build_hasher(&self) -> Self::Hasher {
        seahash::SeaHasher::new()
    }
}