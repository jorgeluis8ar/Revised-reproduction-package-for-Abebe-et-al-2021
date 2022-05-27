
do "do/utilities/dataout.ado"

local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}

use "data/endline_data.dta",replace
keep if time==2

sum $balance

*DEFINE GLOBALS TO CAPTURE OUTCOME FAMILIES
*global outc_employment_end work permanent_work hours_worked earnings work_satisfaction written_agreement self_employed
global outc_employment_end work hours_worked written_agreement permanent_work self_employed earnings work_satisfaction

global outc_jobsearch_end apply_temp apply_perm interview_apply_all offer_apply_all interview_apply_perm offer_apply_perm interview_apply_temp offer_apply_temp cv_application cert_application
global outc_jobquality_end job_by_interview office_work skills_match over_qualified under_qualified
global outc_financial_end expenditure total_savings assets
global outc_expectations_end expect_offer res_wage aspiration expect_job
global outc_mobility_end travel work_away moved_occup moved_in_addis  moved_out_of_addis 
global outc_education_end fulltime_education parttime_education informal_training graduated graduated_vocational graduated_training 
global outc_psychological_end life_satisfaction locus_control oneness  trust
global outc_network_end network_size network_quality guarantor  associations

 
tempfile index_bs
tempfile index_e1

gen n = _n
*ENDLINE INDICES
preserve
foreach outcome_family in outc_employment_end outc_jobsearch_end outc_jobquality_end outc_financial_end outc_expectations_end outc_mobility_end outc_education_end outc_psychological_end outc_network_end {

global outcome_family2 ""
local i = 0
local count = 0
foreach var in $`outcome_family' {
replace `var' = 0 if `var' == .
local i = `i' + 1
if `i' == 1 {
local firstvar = "`var'"
}
corr `firstvar' `var'
matrix corr = r(C)
local exclude = corr[2,1]
if `exclude' != . {
local count = `count' + 1
global outcome_family2 = "$outcome_family2" + " " + "`var'" 
}
egen mean_`var' = mean(`var')
egen sd_cont_`var' = sd(`var') if treat_group == 5
egen minsd_cont_`var' = min(sd_cont_`var')
replace sd_cont_`var' = minsd_cont_`var'
gen norm_`var' = (`var' - mean_`var')/sd_cont_`var'
drop minsd_cont_`var' sd_cont_`var' mean_`var'
}
corr $outcome_family2
matrix SIGMA = r(C)
matrix one = J(`count', 1, 1)
matrix weights = (one'*invsym(SIGMA)*one)*(one'*invsym(SIGMA))

local count = 0
foreach var in $outcome_family2 {
local count = `count' + 1
scalar Aweight = weights[1,`count']
gen weighted_`var' = Aweight*norm_`var'
}

global weighted_vars ""
foreach x in  $outcome_family2 {
	global weighted_vars="$weighted_vars" + " weighted_" +"`x'"
}

egen I`outcome_family' = rowtotal($weighted_vars)
egen miss = rowmiss($weighted_vars)
replace I`outcome_family' = . if miss > 0
drop miss weighted_*
sum I`outcome_family', det
}

keep Ioutc* n  
sort n  
save `index_e1' 
restore

 
* BASELINE INDICES

preserve
foreach outcome_family in outc_employment_end outc_jobsearch_end outc_jobquality_end outc_financial_end outc_expectations_end outc_mobility_end outc_education_end outc_psychological_end outc_network_end {

global outcome_family2 ""
local i = 0
local count = 0
foreach allvar in $`outcome_family' {
local var = "bs_`allvar'"
replace `var' = 0 if `var' == .
local i = `i' + 1
if `i' == 1 {
local firstvar = "`var'"
}
corr `firstvar' `var'
matrix corr = r(C)
local exclude = corr[2,1]
if `exclude' != . {
local count = `count' + 1
global outcome_family2 = "$outcome_family2" + " " + "`var'" 
}
egen mean_`var' = mean(`var')
egen sd_cont_`var' = sd(`var') if treat_group == 5
egen mct_`var' = min(sd_cont_`var')
replace sd_cont_`var' = mct_`var'
gen norm_`var' = (`var' - mean_`var')/sd_cont_`var'
drop mct_`var' sd_cont_`var' mean_`var'
}
corr $outcome_family2
matrix SIGMA = r(C)
matrix one = J(`count', 1, 1)
matrix weights = (one'*invsym(SIGMA)*one)*(one'*invsym(SIGMA))

local count = 0
foreach var in $outcome_family2 {
local count = `count' + 1
scalar Aweight = weights[1,`count']
gen weighted_`var' = Aweight*norm_`var'
}

global weighted_vars ""
foreach x in  $outcome_family2 {
	global weighted_vars="$weighted_vars" + " weighted_" +"`x'"
}

egen  bs_I`outcome_family' = rowtotal($weighted_vars)
egen miss = rowmiss($weighted_vars)
replace bs_I`outcome_family' = . if miss > 0
drop miss weighted_*
sum  bs_I`outcome_family', det
}

keep bs_I* n  
sort n  
save `index_bs' 
restore

capture drop _merge
 merge 1:1 n   using `index_e1'
tab _merge
capture drop _merge
 merge 1:1 n   using `index_bs', 
tab _merge

 

drop _merge

 

*label variable Ioutc_employment_end2 "Employment"       
label variable Ioutc_employment_end "Employment"       
label variable Ioutc_jobsearch_end "Job Search"
label variable Ioutc_jobquality_end "Job Quality"
label variable Ioutc_financial_end "Financial Outcomes"
label variable Ioutc_expectations_end "Expectations and Aspirations"
label variable Ioutc_mobility_end "Mobility"
label variable Ioutc_education_end "Education and Skills"
label variable Ioutc_psychological_end "Wellbeing"
label variable Ioutc_network_end "Networks"

foreach outcome_family in outc_employment_end outc_jobsearch_end outc_jobquality_end outc_financial_end outc_expectations_end outc_mobility_end outc_education_end outc_psychological_end outc_network_end {
winsor I`outcome_family', gen(WI`outcome_family') p(0.01)
winsor bs_I`outcome_family', gen(bs_WI`outcome_family') p(0.01)

}


label variable WIoutc_employment_end "Employment"       
label variable WIoutc_jobsearch_end "Job Search"
label variable WIoutc_jobquality_end "Job Quality"
label variable WIoutc_financial_end "Financial Outcomes"
label variable WIoutc_expectations_end "Expectations and Aspirations"
label variable WIoutc_mobility_end "Mobility"
label variable WIoutc_education_end "Education and Skills"
label variable WIoutc_psychological_end "Wellbeing"
label variable WIoutc_network_end "Networks"
 
 
*DEFINE ONE ADDITIONAL OUTCOME FAMILY TO INCLUDE ALL FAMILY INDICES
global Windices_end   WIoutc_jobquality_end WIoutc_financial_end WIoutc_expectations_end WIoutc_mobility_end WIoutc_education_end WIoutc_psychological_end WIoutc_network_end

capture label var tg_1 "Transport"
capture label var tg_2 "Workshop"
  
capture label var tg_6 "Spillover transport"
capture label var tg_7 "Spillover screening"


gen Transport = ""
gen Screening = ""

gen Spillover_Transport = ""
gen Spollover_Screening = ""

 

local tab 15 
foreach outcome_family in outc_jobsearch_end Windices_end outc_jobquality_end outc_financial_end outc_expectations_end   outc_mobility_end  outc_education_end outc_psychological_end outc_network_end{

global modseq=0

preserve
keep if treat_group == 5
collapse $`outcome_family' [pw=ed_weight]
foreach y in $`outcome_family' {
global modseq=$modseq+1
rename `y' CM_`y' 	
replace CM_`y' = round(CM_`y', 0.001) 
gen idnum_`y' = $modseq 
}
 
keep in 1
keep CM_* idnum_*
gen counter = 1
reshape long CM idnum, i(counter) j(DepVar) string
drop counter 
tempfile CMean
sort idnum
save `CMean', replace
restore

preserve
gen DepVar = ""

global tflist ""
global modseq=0
foreach y in $`outcome_family' {
global modseq=$modseq+1
local m = $modseq
tempfile tfcur

capture gen bs_`y' = .
sum bs_`y'
local exclude = r(mean)


if `exclude' == . {
 parmby "ivreg2 `y' tg_* $balance [pw=ed_weight], partial($balance) cluster(cluster_id)", ylabel label command format(estimate /*min95 max95 %8.2f*/ p stderr /*%8.1e*/) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
 
local N_`m' = e(N)
 test tg_1 = tg_2
local F_`m' = r(F)
local F_p`m' = r(p)
}
else {
 parmby "ivreg2 `y' tg_* bs_`y' $balance [pw=ed_weight], partial($balance) cluster(cluster_id)", ylabel label command format(estimate /*min95 max95 %8.2f*/ p stderr /*%8.1e*/) idn($modseq) saving(`"`tfcur'"', replace) flist(tflist)
  local N_`m' = e(N)
 test tg_1 = tg_2
local F_`m' = r(F)
local F_p`m' = r(p)
}
}
drop _all
append using $tflist
sort idnum command parmseq
describe
keep if parm == "tg_1" | parm == "tg_2" | parm == "tg_3" | parm == "tg_4" | parm == "tg_6" | parm == "tg_7"
 
/**/
tempfile temp_before_q
sort idnum command parmseq
save `temp_before_q', replace

foreach x in 1 2 {

	use `temp_before_q', clear
	keep if parm == "tg_`x'"
	rename p pval
	
*******************************
	* Find q-value (code from Anderson: https://are.berkeley.edu/~mlanderson/ARE_Website/Research.html)
*******************************

	quietly sum pval
	local totalpvals = r(N)
	* Sort the p-values in ascending order and generate a variable that codes each p-value's rank
	quietly sort pval
	quietly gen int rank = _n if pval~=.
	* Set the initial counter to 1 
	local qval = 1
	* Generate the variable that will contain the BKY (2006) sharpened q-values
	gen bky06_qval = 1 if pval~=.
	* Set up a loop that begins by checking which hypotheses are rejected at q = 1.000, then checks which hypotheses are rejected at q = 0.999, then checks which hypotheses are rejected at q = 0.998, etc.  The loop ends by checking which hypotheses are rejected at q = 0.001.
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
		
	}
	
		rename bky06_qval q	
		keep q idnum parmseq
		sort idnum parmseq
		save temp`x', replace
}

		use `temp_before_q', clear
		foreach x in 1 2   {
		capture drop _merge
		merge 1:1 idnum parmseq using temp`x', update
		sort idnum parmseq
		erase temp`x'.dta
		}		
/**/

replace estimate = round(estimate, 0.001)
replace p = round(p, 0.001)                 
replace q = round(q, 0.001)

gen asteriskp = "*" if p <=.10
replace asteriskp = "**" if p <= .05
replace asteriskp = "***" if p <= .01

gen asteriskq = "*" if q <=.10
replace asteriskq = "**" if q <= .05
replace asteriskq = "***" if q <= .01

replace p = round(stderr, 0.001)                 

tostring estimate p, replace force
tostring estimate q, replace force format(%9.0g)
*replace p = "("+p+ "," + q + ")"
replace p = "("+p+")"+asteriskp
replace q = "["+q+"]"+asteriskq

keep idnum ylabel parm estimate p q
sort idnum parm
reshape wide estimate p q, i(idnum) j(parm) string 
expand 4
by idnum, sort: gen num = _n

capture gen Transport = estimatetg_1 if num == 1
capture replace Transport = ptg_1 if num == 2
capture replace Transport = qtg_1 if num == 3
capture replace Transport = "" if num == 4

capture gen Screening = estimatetg_2 if num == 1
capture replace Screening = ptg_2 if num == 2
capture replace Screening = qtg_2 if num == 3
capture replace Screening = "" if num == 4

capture gen Spill_Trans = estimatetg_6 if num == 1
capture replace Spill_Trans = ptg_6 if num == 2
capture replace Spill_Trans = qtg_6 if num == 3
capture replace Spill_Trans = "" if num == 4

capture gen Spill_Screen = estimatetg_7 if num == 1
capture replace Spill_Screen = ptg_7 if num == 2
capture replace Spill_Screen = qtg_7 if num == 3
capture replace Spill_Screen = "" if num == 4


sort idnum
merge idnum using `CMean'
drop _merge

gen F_Sc_Fa_ScFa = .
capture replace F_Sc_Fa_ScFa = `F_p1' if idnum == 1 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p2' if idnum == 2 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p3' if idnum == 3 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p4' if idnum == 4 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p5' if idnum == 5 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p6' if idnum == 6 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p7' if idnum == 7 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p8' if idnum == 8 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p9' if idnum == 9 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p10' if idnum == 10 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p11' if idnum == 11 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p12' if idnum == 12 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p13' if idnum == 13 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p14' if idnum == 14 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p15' if idnum == 15 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p16' if idnum == 16 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p17' if idnum == 17 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p18' if idnum == 18 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p19' if idnum == 19 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p20' if idnum == 20 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p21' if idnum == 21 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p22' if idnum == 22 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p23' if idnum == 23 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p24' if idnum == 24 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p25' if idnum == 25 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p26' if idnum == 26 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p27' if idnum == 27 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p28' if idnum == 28 & num == 1
capture replace F_Sc_Fa_ScFa = `F_p29' if idnum == 29 & num == 1

gen N = .
capture replace N = `N_1' if idnum == 1 & num == 1
capture replace N = `N_2' if idnum == 2 & num == 1
capture replace N = `N_3' if idnum == 3 & num == 1
capture replace N = `N_4' if idnum == 4 & num == 1
capture replace N = `N_5' if idnum == 5 & num == 1
capture replace N = `N_6' if idnum == 6 & num == 1
capture replace N = `N_7' if idnum == 7 & num == 1
capture replace N = `N_8' if idnum == 8 & num == 1
capture replace N = `N_9' if idnum == 9 & num == 1
capture replace N = `N_10' if idnum == 10 & num == 1
capture replace N = `N_11' if idnum == 11 & num == 1
capture replace N = `N_12' if idnum == 12 & num == 1
capture replace N = `N_13' if idnum == 13 & num == 1
capture replace N = `N_14' if idnum == 14 & num == 1
capture replace N = `N_15' if idnum == 15 & num == 1
capture replace N = `N_16' if idnum == 16 & num == 1
capture replace N = `N_17' if idnum == 17 & num == 1
capture replace N = `N_18' if idnum == 18 & num == 1
capture replace N = `N_19' if idnum == 19 & num == 1
capture replace N = `N_20' if idnum == 20 & num == 1
capture replace N = `N_21' if idnum == 21 & num == 1
capture replace N = `N_22' if idnum == 22 & num == 1
capture replace N = `N_23' if idnum == 23 & num == 1
capture replace N = `N_24' if idnum == 24 & num == 1
capture replace N = `N_25' if idnum == 25 & num == 1
capture replace N = `N_26' if idnum == 26 & num == 1
capture replace N = `N_27' if idnum == 27 & num == 1
capture replace N = `N_28' if idnum == 28 & num == 1
capture replace N = `N_29' if idnum == 29 & num == 1

replace ylabel = "" if num == 2 | num == 3 | num == 4
replace CM = . if num == 2 | num == 3 | num ==4
rename ylabel Dep_Var
tostring CM, format(%9.0g) g(ContMean) force
tostring F_Sc_Fa_ScFa, format(%9.0g) replace force
tostring N, format(%9.0g) replace force
replace ContMean = "" if ContMean == "."
replace F_Sc_Fa_ScFa = "" if F_Sc_Fa_ScFa == "."
replace N = "" if N == "."

rename ContMean ControlMean
rename Screening JobAppWorkshop
rename F_Sc_Fa_ScFa F

keep Dep_Var Transport JobAppWorkshop   ControlMean F N
order Dep_Var Transport JobAppWorkshop   ControlMean F N

dataout, save(results/tables/table_a`tab') tex  replace
insheet using results/tables/table_a`tab'.tex, clear
drop in 1/3
gen n = _n
drop if n == _N
drop n
*replace v1 = "\multicolumn{1}{l}{\emph{Outcome}} & \multicolumn{1}{c}{Transport} &\multicolumn{1}{c}{Job App. Workshop} & \multicolumn{1}{c}{Spillover 1} & \multicolumn{1}{c}{Spillover 2} & \multicolumn{1}{c}{Control Mean} & \multicolumn{1}{c}{F} & \multicolumn{1}{c}{N} \\ \hline \\" in 2
replace v1 = "\multicolumn{1}{l}{\emph{Outcome}} & \multicolumn{1}{c}{Transport} &\multicolumn{1}{c}{Job App. Workshop} & \multicolumn{1}{c}{Control Mean} & \multicolumn{1}{c}{F} & \multicolumn{1}{c}{N} \\ \hline \\" in 2
*gen n = _n
*expand 2 in 2
*sort n
*replace v1 = "&  &  &  &  &  &  &  \\" in 3
outsheet  	v1	using "results/tables/table_a`tab'.tex", noname noquote replace
restore
local tab = `tab'+1
}
exit
