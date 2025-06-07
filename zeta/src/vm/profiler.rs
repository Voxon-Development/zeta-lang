use std::collections::HashMap;

pub type TimeTaken = u64;

pub struct Profiler {
    call_counts: HashMap<u64, FunctionCall>
}

#[derive(Copy, Clone)]
pub struct FunctionCall {
    pub count: u64,
    pub time_taken: TimeTaken
}

impl Profiler {
    pub fn new() -> Profiler {
        Profiler { call_counts: HashMap::new() }
    }
    
    pub fn new_with_function_counts(call_counts: HashMap<u64, FunctionCall>) -> Profiler {
        Profiler { call_counts }
    }
    
    pub fn new_with_function_count(count: usize) -> Profiler {
        Profiler {
            call_counts: HashMap::with_capacity(count)
        }
    }
    
    pub fn reset(&mut self) {
        self.call_counts = HashMap::new();
    }

    pub fn record_call(&mut self, function_id: u64, time_taken: TimeTaken) {
        if self.call_counts.contains_key(&function_id) {
            let mut call_count = self.call_counts.get(&function_id).unwrap().clone();
            call_count.count += 1;
            call_count.time_taken = time_taken;
            self.call_counts.insert(function_id, call_count);
            return;
        }
        
        self.call_counts.insert(function_id, FunctionCall { count: 1, time_taken });
    }

    pub fn get_call_counts(&self) -> &HashMap<u64, FunctionCall> {
        &self.call_counts
    }

    pub fn get_call_counts_mut(&mut self) -> &mut HashMap<u64, FunctionCall> {
        &mut self.call_counts
    }

    pub fn get_func_call_count(&self, index: u64) -> &FunctionCall {
        self.call_counts.get(&index).unwrap()
    }
}