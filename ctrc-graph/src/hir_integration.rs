use crate::DestructorCallType;
use crate::hir_integration::hir::HirModuleWithCTRC;
use std::sync::Arc;
use zetaruntime::string_pool::StringPool;
use zetaruntime::bump::GrowableBump;
use ir::hir;
use ir::hir::{HirModule, DropInsertion, DestructorCall, DropLocation, CTRCAnalysisResult};
use crate::ctrc_pvg_graph::{CTRCAnalysisResult as CTRCGraphResult, analyze_hir_for_ctrc};

/// Integration API for CTRC analysis with HIR pretty printing
pub struct CTRCHirIntegration;

impl CTRCHirIntegration {
    /// Analyze HIR module and return it with CTRC analysis results
    pub fn analyze_hir_module<'a, 'bump>(
        module: HirModule<'a, 'bump>,
        bump: &'bump GrowableBump<'bump>,
    ) -> HirModuleWithCTRC<'a, 'bump> 
    where
        'bump: 'a,
    {
        let ctrc_result: crate::CTRCAnalysisResult = analyze_hir_for_ctrc(&module, bump);
        let hir_ctrc_result: CTRCAnalysisResult = Self::convert_ctrc_result(ctrc_result);
        
        HirModuleWithCTRC {
            module,
            ctrc_analysis: Some(hir_ctrc_result),
        }
    }
    
    /// Convert CTRC graph analysis result to HIR-integrated format
    fn convert_ctrc_result(ctrc_result: CTRCGraphResult) -> CTRCAnalysisResult {
        CTRCAnalysisResult {
            structs_with_destructors: ctrc_result.structs_with_destructors,
            droppable_fields: ctrc_result.droppable_fields,
            variable_aliases: ctrc_result.variable_aliases.into_iter().collect(),
            allocation_sites: ctrc_result.allocation_sites.into_iter().collect(),
            drop_insertions: Vec::into_iter(ctrc_result.drop_insertions)
                .map(|drop| DropInsertion {
                    program_point: drop.program_point,
                    variable_name: drop.variable_name,
                    alias_id: drop.alias_id,
                    location_hint: Some(DropLocation::EndOfBlock), // Default hint
                })
                .collect(),
            destructor_calls: Vec::into_iter(ctrc_result.destructor_calls)
                .map(|call| DestructorCall {
                    program_point: call.program_point,
                    alias_id: call.alias_id,
                    call_type: match call.call_type {
                        DestructorCallType::AutoDrop => hir::DestructorCallType::AutoDrop,
                        DestructorCallType::ExplicitDrop => hir::DestructorCallType::ExplicitDrop,
                        DestructorCallType::ScopeDrop => hir::DestructorCallType::ScopeDrop,
                    },
                    location_hint: Some(DropLocation::EndOfBlock), // Default hint
                })
                .collect(),
            potential_leaks: ctrc_result.potential_leaks,
        }
    }
    
    pub fn create_hir_with_ctrc<'a, 'bump>(
        module: HirModule<'a, 'bump>,
        ctrc_result: Option<CTRCAnalysisResult>,
    ) -> HirModuleWithCTRC<'a, 'bump> 
    where
        'bump: 'a,
    {
        HirModuleWithCTRC {
            module,
            ctrc_analysis: ctrc_result,
        }
    }
    
    /// Verify that CTRC analysis results are memory safe
    pub fn verify_memory_safety(module_with_ctrc: &HirModuleWithCTRC<'_, '_>) -> bool {
        if let Some(ctrc) = &module_with_ctrc.ctrc_analysis {
            !ctrc.has_memory_safety_issues()
        } else {
            true // No analysis means we assume it's safe
        }
    }
    
    /// Get summary statistics for CTRC analysis
    pub fn get_analysis_summary(module_with_ctrc: &HirModuleWithCTRC<'_, '_>) -> CTRCAnalysisSummary {
        if let Some(ctrc) = &module_with_ctrc.ctrc_analysis {
            CTRCAnalysisSummary {
                structs_with_destructors: ctrc.structs_with_destructors.len(),
                drop_insertions: ctrc.drop_insertions.len(),
                destructor_calls: ctrc.destructor_calls.len(),
                potential_leaks: ctrc.potential_leaks.len(),
                allocation_sites: ctrc.allocation_sites.len(),
                variable_aliases: ctrc.variable_aliases.len(),
                memory_safe: !ctrc.has_memory_safety_issues(),
            }
        } else {
            CTRCAnalysisSummary::default()
        }
    }
}

/// Summary statistics for CTRC analysis
#[derive(Debug, Clone, Default)]
pub struct CTRCAnalysisSummary {
    pub structs_with_destructors: usize,
    pub drop_insertions: usize,
    pub destructor_calls: usize,
    pub potential_leaks: usize,
    pub allocation_sites: usize,
    pub variable_aliases: usize,
    pub memory_safe: bool,
}

impl CTRCAnalysisSummary {
    pub fn has_analysis_data(&self) -> bool {
        self.structs_with_destructors > 0 
            || self.drop_insertions > 0 
            || self.destructor_calls > 0 
            || self.allocation_sites > 0
    }
}

pub mod convenience {
    use super::*;
    use ir::pretty::IrPrettyPrinter;
    
    pub fn analyze_and_pretty_print<'a, 'bump>(
        module: HirModule<'a, 'bump>,
        bump: &'bump GrowableBump<'bump>,
        string_pool: Arc<StringPool>,
    ) -> Result<String, std::fmt::Error> 
    where
        'bump: 'a,
    {
        let module_with_ctrc = CTRCHirIntegration::analyze_hir_module(module, bump);
        IrPrettyPrinter::hir_with_ctrc_to_string(string_pool, &module_with_ctrc)
    }
    
    pub fn pretty_print_with_ctrc<'a, 'bump>(
        module_with_ctrc: &HirModuleWithCTRC<'a, 'bump>,
        string_pool: Arc<StringPool>,
    ) -> Result<String, std::fmt::Error> {
        IrPrettyPrinter::hir_with_ctrc_to_string(string_pool, module_with_ctrc)
    }
}
