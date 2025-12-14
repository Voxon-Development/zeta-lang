use std::collections::HashMap;
use smallvec::SmallVec;
use zetaruntime::string_pool::StringPool;
use ir::errors::reporter::{ErrorReporter, CTRCLeakInfo};
use ir::hir::StrId;
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
        
        let span = SourceSpan::new(file_name, 1, 1);

        const START: &'static str = "Potential memory leak detected for variable '";
        const END: &'static str = "'";

        let mut buf: SmallVec<u8, 64> = SmallVec::with_capacity(START.len() + variable_name.len() + END.len());
        buf.extend_from_slice(START.as_bytes());
        buf.extend_from_slice(variable_name.as_bytes());
        buf.extend_from_slice(END.as_bytes());
        
        reporter.add_ctrc_error(
            StrId::from(self.string_pool.intern_bytes(buf.as_slice())),
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
                StrId::from(self.string_pool.intern("Potential reference cycle detected")),
                Some(span),
                Some(leak_info),
            );
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

                // Fast alternative to interning `String` directly.
                const START: &'static str = "Potential double-free detected for variable '";
                const MIDDLE: &'static str = "' (dropped ";
                const END: &'static str = " times)";

                let string = count.to_string();
                let bytes = string.as_bytes();

                let mut buf: SmallVec<u8, 64> = SmallVec::with_capacity(START.len() + variable_name.len() + MIDDLE.len() + bytes.len() + END.len());
                buf.extend_from_slice(START.as_bytes());
                buf.extend_from_slice(variable_name.as_bytes());
                buf.extend_from_slice(MIDDLE.as_bytes());
                buf.extend_from_slice(bytes);
                buf.extend_from_slice(END.as_bytes());
                
                reporter.add_ctrc_error(
                    StrId::from(self.string_pool.intern_bytes(buf.as_slice())),
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

            const START: &'static str = "Module '";
            const MIDDLE: &'static str = "' has ";
            const END: &'static str = " potential memory safety issues";

            let string = result.potential_leaks.len().to_string();
            let bytes = string.as_bytes();
            let mut buf: SmallVec<u8, 64> = SmallVec::with_capacity(START.len() + file_name.len() + MIDDLE.len() + bytes.len() + END.len());
            buf.extend_from_slice(START.as_bytes());
            buf.extend_from_slice(file_name.as_bytes());
            buf.extend_from_slice(MIDDLE.as_bytes());
            buf.extend_from_slice(bytes);
            buf.extend_from_slice(END.as_bytes());

            reporter.add_ctrc_error(
                StrId::from(self.string_pool.intern_bytes(buf.as_slice())),
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
