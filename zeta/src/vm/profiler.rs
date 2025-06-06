pub struct Profiler {
    call_counts: Vec<u64>
}

impl Profiler {
    pub fn new() -> Profiler {
        Profiler { call_counts: Vec::new() }
    }

    pub fn record_call(&mut self) {
        self.call_counts.push(1);
    }

    pub fn get_call_counts(&self) -> &Vec<u64> {
        &self.call_counts
    }

    pub fn get_call_counts_mut(&mut self) -> &mut Vec<u64> {
        &mut self.call_counts
    }

    pub fn get_total_call_count(&self) -> u64 {
        self.call_counts.iter().sum()
    }

    pub fn get_func_call_count(&self, index: usize) -> u64 {
        self.call_counts[index]
    }
}