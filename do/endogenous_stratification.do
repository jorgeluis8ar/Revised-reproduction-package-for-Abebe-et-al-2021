* This do-file implements the Abadie et al estimator, as reported in 
* Tables 4, 5 and 6.
 
set more off

clear

local keeplist  monthly_wage  

****Using the final dataset, recreate the dataset with just endline earnings 

use "data/endline_data.dta", clear
keep if treat_groupind==1|treat_groupind==2|treat_groupind==5
keep if  time==6

tempfile SmallLongrunData

keep 	r_id  `keeplist'

foreach varname of varlist `keeplist'  ///
	{
		rename 	`varname' end_`varname'
	}

sort 	r_id
save 	`SmallLongrunData', replace

global factor7 	"bs_age bs_female bs_married bs_oromo bs_amhara bs_migrant_birth bs_education_dummy2 bs_education_dummy3 bs_education_dummy4 bs_years_since_school bs_live_parents bs_stad_dist bs_travel bs_search_freq bs_search_6months bs_apply_perm bs_work_contract bs_work_cas bs_work_temp bs_self_employed bs_experience_perm bs_cert_application bs_cv_application bs_saving_dummy bs_expenditure bs_res_wage"


*Open the final dataset, and now work with the first endline (time=2) to generate predicted earnings
use "data/endline_data.dta", clear
keep if treat_groupind==1|treat_groupind==2|treat_groupind==5
rename tg_1 t_1 
rename tg_2 t_2
drop tg_6 tg_7

keep if time ==2

foreach varname of varlist `keeplist'   ///
	{
		gen bs_end_`varname' 	= bs_`varname'
	}

sort 	r_id
merge 1:1	r_id 	using `SmallLongrunData'
drop 	if _merge == 2
drop 	_merge

*label variables
*do 	LabelVariables 
rename bs_respondent_age bs_age
gen bs_education_dummy2 =  bs_education==1
  gen bs_education_dummy3 =  bs_education==2
  gen bs_education_dummy4 =  bs_education==3
  
  
qui{
label var bs_search_freq 		"Baseline: Search frequency"
label var bs_years_since 		"Baseline: Years since finished formal education"
label var bs_age 				"Baseline: Age"
label var bs_search_6months  	"Baseline: Dummy: Searched for work in the last 6 months"
label var bs_experience_perm 	"Baseline: Dummy: Respondent has work experience in a permanent job"
label var bs_saving_dummy 		"Baseline: Dummy: Savings is above the median"
label var bs_married 			"Baseline: Dummy: Married"
label var bs_live_parents 		"Baseline: Dummy: Respondent lives with mother or father"
label var bs_oromo 				"Baseline: Dummy: Respondent is ethnically Oromo"
label var bs_amhara 			"Baseline: Dummy: Respondent is ethnically Amhara"
label var bs_migrant_birth 		"Baseline: Dummy: Born outside of Addis"
label var bs_female 			"Baseline: Dummy: Female"
label var bs_apply_perm 		"Baseline: Number of applications for permanent jobs made in the last 12 months"
label var bs_cert_application 	"Baseline: Dummy: Respondent has certificates used for job applications"
label var bs_cv_application 	"Baseline: Dummy: Respondent has a CV used for job applications"
label var bs_expenditure 		"Baseline: Total expenditure in last 7 days"
label var bs_travel 			"Baseline: Number of trips to central Addis Ababa in last 7 days"
label var bs_work_contract 		"Baseline: Contract worker"
label var bs_work_cas 			"Baseline: Casual worker"
label var bs_work_temp 			"Baseline: Temporarily employed"
label var bs_self_employed 		"Baseline: Self-employed"
label var bs_education_dummy2 	"Baseline: Dummy: Vocational education (reference: high school)"
label var bs_education_dummy3 	"Baseline: Dummy: Diploma (reference: high school)"
label var bs_education_dummy4 	"Baseline: Dummy: Degree (reference: high school)"
label var bs_stad_dist 			"Baseline: Distance to city centre"
label var bs_res_wage 			"Baseline: Reservation wage"
}

** FIRST, RUN THE BASIC BALANCE TEST, FOR THE PREDICTION ACROSS THE WHOLE SAMPLE...
sum $factor7

gen ControlGroup	= (treat_groupind==5)
 
eststo clear

eststo: reg 	end_monthly_wage $factor7 [pw = ed_weight] if ControlGroup

esttab using "Tables/AbadieFirstStage.tex", b(%9.3f) se(%9.3f) ///
			label staraux star(* 0.1 ** 0.05 *** 0.01) width(1in) replace

predict y_hat

tempfile AbadieSmallEarnings
save 	`AbadieSmallEarnings', replace

quietly su y_hat, de
gen 	high_e 	= (y_hat >= `r(p50)') 	if y_hat != .
gen 	low_e 	= (y_hat < 	`r(p50)') 	if y_hat != .

	
** Now manually work out the control group means...
foreach varname of varlist end_monthly_wage   permanent_work written_agreement ///
	{
		su 	`varname' 	if high_e 	& ControlGroup
		su 	`varname' 	if low_e 	& ControlGroup
		
		set more off
	}

	foreach v in  1 2    ///
		{
			ge high_e_t_`v' = t_`v'* high_e
			ge low_e_t_`v' = t_`v'* low_e
		}


*SF: Here I'm just checking the main results without bootstrapping. This creates the last row of Tables 4, 5, 6 respectively. 
*This looks mostly good. The sample size is now slightly larger than in the older versions, and coefficents change very slightly too. 
foreach OutcomeVariable of varlist end_monthly_wage  permanent_work written_agreement   ///
	{
	
	  reg `OutcomeVariable' high_e_t* low_e_t* high_e bs_`OutcomeVariable' [pw=ed_weight]   

	  test   high_e_t_1 == low_e_t_1
	  
	  *check equality of transport treatments
	  test   high_e_t_2 == low_e_t_2 
	  display e(N)
	}


** DEFINE THE PROGRAMS... **
capture program drop 	SplitSampleProgram
capture program drop 	RunSplitSample

program SplitSampleProgram

	args 	OutcomeVariable
	
	capture drop MyRandom
	capture drop PredictionGroup
	capture drop EstimationGroup
	capture drop high_e* low_e*
	capture drop p_hat
	
	gen 	MyRandom = uniform()

	sort 	ControlGroup bs_education MyRandom
	bysort 	ControlGroup bs_education: 		gen PredictionGroup = (_n <= 0.5 * _N)

	replace PredictionGroup 	= 0 	if ControlGroup == 0
	gen 	EstimationGroup 	= (1 - PredictionGroup)

	capture drop y_hat

	reg 	end_monthly_wage $factor7 [pw = ed_weight] if PredictionGroup
	predict y_hat
	
	quietly su y_hat, de
	gen 	high_e 	= (y_hat >= `r(p50)') 	if y_hat != .
	gen 	low_e 	= (y_hat < 	`r(p50)') 	if y_hat != .

	foreach v in  1 2 ///
		{
			ge high_e_t_`v' = t_`v'* high_e
			ge low_e_t_`v' = t_`v'* low_e
		}


	reg `OutcomeVariable' high_e_t* low_e_t* high_e bs_`OutcomeVariable' [pw=ed_weight]   

end

program RunSplitSample

	args 	OutcomeVariable

	preserve

	simulate _b, reps($InnerReps): SplitSampleProgram `OutcomeVariable'
	collapse (mean) _b*
	
	matrix b = [_b_high_e_t_1[1], _b_high_e_t_2[1], _b_low_e_t_1[1], _b_low_e_t_2[1], _b_high_e[1], _b_cons[1]]
	
	ereturn post b
	
	restore

end

global 	InnerReps 	= 100
global 	OuterReps 	= 1000

		******  ESTIMATION AND OUTPUT *****

eststo clear

		** TABLE 4 **

eststo: bootstrap, reps($OuterReps) cluster(cluster_id): RunSplitSample end_monthly_wage  

		test c1 == c3
		estadd 	scalar 	TransportEquality 	= r(p)

		test c2 == c4
		estadd 	scalar 	WorkshopEquality 	= r(p)

		test c1 == c2 == c3 == c4 == 0
		estadd 	scalar 	TransWorkEquality 	= r(p)

		test c3 == c4 == 0
		estadd 	scalar 	LowZero 			= r(p)

		test c3 == c4
		estadd 	scalar 	LowEqual 			= r(p)
		
			
		** TABLE 5 **

eststo: bootstrap, reps($OuterReps) cluster(cluster_id): RunSplitSample permanent_work

		test c1 == c3
		estadd 	scalar 	TransportEquality 	= r(p)

		test c2 == c4
		estadd 	scalar 	WorkshopEquality 	= r(p)

		test c1 == c2 == c3 == c4 == 0
		estadd 	scalar 	TransWorkEquality 	= r(p)

		test c3 == c4 == 0
		estadd 	scalar 	LowZero 			= r(p)

		test c3 == c4
		estadd 	scalar 	LowEqual 			= r(p)

		** TABLE 6 **
		
eststo: bootstrap, reps($OuterReps) cluster(cluster_id): RunSplitSample written_agreement

		test c1 == c3
		estadd 	scalar 	TransportEquality 	= r(p)

		test c2 == c4
		estadd 	scalar 	WorkshopEquality 	= r(p)

		test c1 == c2 == c3 == c4 == 0
		estadd 	scalar 	TransWorkEquality 	= r(p)

		test c3 == c4 == 0
		estadd 	scalar 	LowZero 			= r(p)

		test c3 == c4
		estadd 	scalar 	LowEqual 			= r(p)

esttab using "Tables/table_4_5_6_finalrows.tex", b(%9.3f) se(%9.3f) ///
			label staraux star(* 0.1 ** 0.05 *** 0.01) replace width(7in) ///
			stats(N TransportEquality WorkshopEquality TransWorkEquality LowZero LowEqual) nomtitles nonotes compress
		
		
exit

