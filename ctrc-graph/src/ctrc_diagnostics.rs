use std::collections::HashMap;
use zetaruntime::string_pool::StringPool;
use ir::errors::reporter::{ErrorReporter, CTRCLeakInfo};
use ir::span::SourceSpan;
use crate::ctrc_pvg_graph::{CTRCAnalysisResult, AliasID};

pub struct CTRCDiagnostics<'a> {
    string_pool: &'a StringPool,
}

impl<'a> CTRCDiagnostics<'a> {
    pub fn new(string_pool: &'a StringPool) -> Self {
        Self { string_pool }
    }
    
    pub fn analyze_and_report<'bump>(
        &self,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        for leak_alias in &result.potential_leaks {
            self.report_memory_leak(leak_alias, result, reporter, file_name);
        }
        
        self.detect_and_report_cycles(result, reporter, file_name);
        
        self.report_unused_allocations(result, reporter, file_name);
        
        self.report_potential_double_free(result, reporter, file_name);
    }
    
    fn report_memory_leak<'bump>(
        &self,
        leak_alias: &AliasID,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        let allocation_site = result.allocation_sites
            .iter()
            .find(|(_, alias)| *alias == leak_alias)
            .map(|(point, _)| point);
        
        let variable_name = result.variable_aliases
            .iter()
            .find(|(_, alias)| *alias == leak_alias)
            .map(|(var_name, _)| self.string_pool.resolve_string(&*var_name))
            .unwrap_or("<unknown>");
        
        let allocation_site_str = allocation_site
            .map(|point| format!("{}:{}", file_name, point))
            .unwrap_or_else(|| "unknown location".to_string());
        
        let leak_info = CTRCLeakInfo {
            variable_name: variable_name.to_string(),
            allocation_site: allocation_site_str,
            potential_cycles: vec![], // Will be filled by cycle detection
        };
        
        // Create a dummy span for now - in a real implementation, you'd track spans through the analysis
        let span = SourceSpan::new(file_name, 1, 1);
        
        reporter.add_ctrc_error(
            format!("Potential memory leak detected for variable '{}'", variable_name),
            Some(span),
            Some(leak_info),
        );
    }
    
    fn detect_and_report_cycles<'bump>(
        &self,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        // Simple cycle detection based on allocation patterns
        let mut potential_cycles = Vec::new();
        
        // Look for variables that might be involved in cycles
        // This is a simplified heuristic - real cycle detection would be more sophisticated
        for (var_name, alias_id) in &result.variable_aliases {
            let var_str = self.string_pool.resolve_string(&*var_name);
            
            let drop_count = result.drop_insertions
                .iter()
                .filter(|drop| drop.alias_id == *alias_id)
                .count();
            
            let destructor_count = result.destructor_calls
                .iter()
                .filter(|call| call.alias_id == *alias_id)
                .count();
            
            // Heuristic: if we have fewer drops than expected, might be a cycle
            if drop_count < destructor_count {
                potential_cycles.push(var_str.to_string());
            }
        }
        
        if !potential_cycles.is_empty() {
            let span = SourceSpan::new(file_name, 1, 1);
            
            let leak_info = CTRCLeakInfo {
                variable_name: potential_cycles.join(", "),
                allocation_site: "multiple locations".to_string(),
                potential_cycles: potential_cycles.clone(),
            };
            
            reporter.add_ctrc_error(
                "Potential reference cycle detected".to_string(),
                Some(span),
                Some(leak_info),
            );
        }
    }
    
    fn report_unused_allocations<'bump>(
        &self,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        for (allocation_point, alias_id) in &result.allocation_sites {
            let has_drop = result.drop_insertions
                .iter()
                .any(|drop| drop.alias_id == *alias_id);
            
            let has_destructor = result.destructor_calls
                .iter()
                .any(|call| call.alias_id == *alias_id);
            
            if !has_drop && !has_destructor {
                let variable_name = result.variable_aliases
                    .iter()
                    .find(|(_, var_alias)| *var_alias == alias_id)
                    .map(|(var_name, _)| self.string_pool.resolve_string(&*var_name))
                    .unwrap_or("<unknown>");
                
                let span = SourceSpan::new(file_name, *allocation_point, 1);
                
                reporter.add_ctrc_error(
                    format!("Unused allocation for variable '{}' - consider removing or using the value", variable_name),
                    Some(span),
                    None,
                );
            }
        }
    }
    
    fn report_potential_double_free<'bump>(
        &self,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        let mut drop_counts: HashMap<AliasID, usize> = HashMap::new();
        
        for drop in &result.drop_insertions {
            *drop_counts.entry(drop.alias_id).or_insert(0) += 1;
        }
        
        for (alias_id, count) in drop_counts {
            if count > 1 {
                // Find variable name
                let variable_name = result.variable_aliases
                    .iter()
                    .find(|(_, var_alias)| *var_alias == &alias_id)
                    .map(|(var_name, _)| self.string_pool.resolve_string(&*var_name))
                    .unwrap_or("<unknown>");
                
                let span = SourceSpan::new(file_name, 1, 1);
                
                reporter.add_ctrc_error(
                    format!("Potential double-free detected for variable '{}' (dropped {} times)", variable_name, count),
                    Some(span),
                    None,
                );
            }
        }
    }
    
    pub fn generate_memory_safety_summary<'bump>(
        &self,
        result: &CTRCAnalysisResult,
        reporter: &mut ErrorReporter<'a, 'bump>,
        file_name: &'a str,
    ) {
        if result.has_memory_safety_issues() {
            let span = SourceSpan::new(file_name, 1, 1);
            
            reporter.add_ctrc_error(
                format!("Module '{}' has {} potential memory safety issues", 
                       file_name, result.potential_leaks.len()),
                Some(span),
                None,
            );
        }
    }
}

pub fn analyze_ctrc_and_report<'a, 'bump>(
    result: &CTRCAnalysisResult,
    string_pool: &'a StringPool,
    reporter: &mut ErrorReporter<'a, 'bump>,
    file_name: &'a str,
) {
    let diagnostics = CTRCDiagnostics::new(string_pool);
    diagnostics.analyze_and_report(result, reporter, file_name);
}
