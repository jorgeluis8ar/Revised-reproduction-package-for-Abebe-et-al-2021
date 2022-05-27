cd "/Directory here/Revised-reproduction-package-for-Abebe-et-al-2021/Revised-reproduction-package-for-Abebe-et-al-2021/Proposed Replication File"



** -----------------------------------------------------------------------------
**  Loading all functions ------------------------------------------------------
** -----------------------------------------------------------------------------

do "do/Utilities/_DefineQuadraticConstraintProgram.do"
do "do/Utilities/_DefineQuadraticConstraintProgram_monthly.do"
do "do/Utilities/_attrition_tables.do"
do "do/Utilities/_itt_bothendlines.do"
do "do/Utilities/_itt_het.do"
do "do/Utilities/_itt_oneendline.do"
do "do/Utilities/_itt_onetreat.do"
do "do/Utilities/dataout.ado"
do "do/Utilities/dataout.sthlp"


** -----------------------------------------------------------------------------
**  Running all results --------------------------------------------------------
** -----------------------------------------------------------------------------

do "do/Results/bounds.do"
do "do/Results/comparison_other_studies.do"
do "do/Results/complete_family_tables_a15_a23.do"
do "do/Results/endogenous_stratification.do"
do "do/Results/main_endline_results.do"
do "do/Results/mediation_analysis.do"
do "do/Results/premia.do"
do "do/Results/rdd_a14.do"
do "do/Results/spillovers.do"
do "do/Results/trajectories.do"
