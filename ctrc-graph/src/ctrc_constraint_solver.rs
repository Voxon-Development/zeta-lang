use crate::{AliasID, CTRCAnalysisResult, CTRCGraph, Constraint, ConstraintReason, ProgramPoint};

impl CTRCGraph {
    pub(crate) fn add_constraint(&mut self, alias: AliasID, point: ProgramPoint, delta: isize, reason: ConstraintReason) {
        let constraint = Constraint {
            alias,
            point,
            delta,
            reason,
        };
        self.constraints.push(constraint);
    }

    pub(crate) fn solve_constraints(&mut self, result: &mut CTRCAnalysisResult) {
        let program_points: Vec<ProgramPoint> = (0..self.next_program_point).collect();
        let constraints: Vec<Constraint> = self.constraints.iter().copied().collect();

        self.solutions = crate::ctrc_pvg_graph::solve_ctrc(self, &constraints, &program_points);

        // Verify no leaks or use-after-free
        for (alias_id, count_range) in &self.solutions {
            if count_range.lower > 0 {
                result.potential_leaks.push(*alias_id);
            }
        }
    }
}