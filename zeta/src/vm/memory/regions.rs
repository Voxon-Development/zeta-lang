use ahash::HashMap;
use ir::VMValue;

type RegionId = usize;

#[derive(Debug, Clone)]
pub struct Region {
    pub id: RegionId,
    pub allocations: HashMap<String, VMValue>,
}

pub struct RegionAllocator {
    next_id: usize,
    regions: HashMap<RegionId, Region>
}

impl RegionAllocator {
    #[inline]
    pub fn new() -> Self {
        Self {
            next_id: 0,
            regions: HashMap::default(),
        }
    }

    pub fn new_region(&mut self) -> Region {
        let id = self.next_id;
        self.next_id += 1;
        Region {
            id,
            allocations: HashMap::default(),
        }
    }

    pub fn get_region(&self, id: RegionId) -> Option<&Region> {
        self.regions.get(&id)
    }
    
    pub fn alloc_in(&mut self, region_id: RegionId, var: String, value: VMValue) {
        if let Some(region) = self.regions.get_mut(&region_id) {
            region.allocations.insert(var, value);
        } else {
            panic!("Tried to allocate in non-existent region {region_id}");
        }
    }

    pub fn pop_region(&mut self, id: RegionId) {
        self.regions.remove(&id);
    }

    pub fn is_region_valid(&self, id: RegionId) -> bool {
        self.regions.contains_key(&id)
    }
}
