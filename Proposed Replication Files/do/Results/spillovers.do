
local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size

global balance "" 

foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}

 
use "data/endline_data.dta",replace
keep if time==2
 
 
global binarylist work hours_worked written_agreement permanent_work self_employed earnings work_satisfaction

**Saturation in the transport treatment. 
keep if treat_group==1|treat_group==5|treat_group==6

tab treat_groupind rs_amount
tab rs_amount, gen(rs_)
rename rs_1 rs_20
rename rs_2 rs_40 
rename rs_3 rs_75
rename rs_4 rs_90

rename tg_6 spill
rename tg_1 treat

foreach y in  20 40 75 90{
gen treat_`y' = treat*rs_`y'
gen spill_`y' = spill*rs_`y'

replace treat_`y' = 0 if treat_group==5
replace spill_`y' = 0 if treat_group==5

 label var treat_`y' "`y'%"
label var  spill_`y' "`y'%"

}

gen control = treat_group==5
drop treat_groupind treat_group
*work_satisfaction written_agreement self_employed job_by_interview office_work skills_match over_qualified under_qualified cv_application cert_application 
tempfile temp 
save `temp'

local tab 35 

foreach group in spill treat {
 use `temp',clear
 
	foreach y in $binarylist{
	preserve

	capture gen bs_`y' = .
	sum bs_`y'
	local exclude = r(mean)

	if `exclude' == . { 
	global baseline 
	}
	else{
	global baseline bs_`y'
	}

	ivreg2 `y' spill_* treat_* $balance bs_`y' [pw=ed_weight], partial($balance) cluster(cluster_id)
	/*
	
	The previuos regression uses the command for Instrumental variables, but as it does no defines intruments,
	the second first stage is not estimated and thus the command estimates a simple ols linear regression. So why 
	do the author do that? The reason is because they are using the option partial. What this option does is that
	it uses the Frisch-Waugh-Lovell (FWL) theorem and partials out the coefficients of the regression and the ones
	that are kept still have the same standard error and point estimate.
	
	Finally, the regression estimates the spillover effects of the transport treatment on the treated by randomised
	level of cluster saturation

	*/
	if "`group'"=="spill"{
	test spill_20 = spill_40 = spill_75 =spill_90
	local spill_fp = r(p)
	local spill_F = r(F)

	}
	else{
	test treat_20 = treat_40 = treat_75 =treat_90
	local spill_fp = r(p)
	local spill_F = r(F)
	}
	
	keep in 1
	gen a1 = _b[`group'_20]
	gen a2 = _se[`group'_20]
	gen b1 = _b[`group'_40]
	gen b2 = _se[`group'_40]
	gen c1 = _b[`group'_75]
	gen c2 = _se[`group'_75]
	gen d1 = _b[`group'_90]
	gen d2 = _se[`group'_90]
	keep a1 a2 b1 b2 c1 c2 d1 d2 

	foreach var in a b c d {
	gen flag_`var'= 1 if abs(`var'1 / `var'2) >= 1.645
	replace flag_`var'= 2 if abs(`var'1 / `var'2) >= 1.96
	replace flag_`var'= 3 if abs(`var'1 / `var'2) >= 2.58
	}

	tostring _all, replace force format(%9.3f)
	foreach var in a b c d {
	replace `var'2 = "(" + `var'2 + ")*" if flag_`var' == "1.000"
	replace `var'2 = "(" + `var'2 + ")**" if flag_`var' == "2.000"
	replace `var'2 = "(" + `var'2 + ")***" if flag_`var' == "3.000"
	replace `var'2 = "(" + `var'2 + ")" if flag_`var' == "."
	}
	gen count = 1
	reshape long a b c d, i(count) j(counter)
	gen spill_fp = `spill_fp' if counter == 1
	gen flag_spill = 1 if spill_fp < .10
	replace flag_spill = 2 if spill_fp < .05
	replace flag_spill = 3 if spill_fp < .01
	tostring spill_fp, format(%9.3f) replace force
	replace spill_fp = spill_fp + "*" if flag_spill == 1
	replace spill_fp = spill_fp + "**" if flag_spill == 2
	replace spill_fp = spill_fp + "***" if flag_spill == 3
	replace spill_fp = "" if spill_fp == "."
	gen outc = "`y'" if counter == 1
	keep outc a b c d spill_fp
	order outc a b c d spill_fp
	list
	save temp_`y', replace
	restore
	}

	clear
	foreach y in $binarylist{
	append using temp_`y'
	}


	li

	foreach y in $binarylist{
	erase temp_`y'.dta
	}

replace outc = "Worked" if outc == "work"
replace outc = "Hours worked" if outc == "hours_worked"
replace outc = "Formal work" if outc == "written_agreement"
replace outc = "Perm. work" if outc == "permanent_work"
replace outc = "Self-employed" if outc == "self_employed"
replace outc = "Monthly earnings" if outc == "earnings"
replace outc = "Satis. with work" if outc == "work_satisfaction"

dataout, save("results/tables/table_a`tab'") tex excel replace
insheet using "results/tables/table_a`tab'.tex" ,clear

drop in 1/3
gen n = _n
drop if n == _N
drop n
replace v1 = "\begin{tabular}{lccccc} \hline \hline" in 1

replace v1 = "\multicolumn{1}{l}{} & \multicolumn{1}{c}{20\%} &\multicolumn{1}{c}{40\%} & \multicolumn{1}{c}{75\%} & \multicolumn{1}{c}{90\%} & \multicolumn{1}{c}{\emph{F(p)}} \\ \hline" in 2

outsheet  	v1	using "results/tables/table_a`tab'.tex", noname noquote replace
local tab = `tab'+1
 
}




*********************************************************************************************************8

* table a.34 spillovers of both interventions on employment outcomes. 

*********************************************************************************************************
 
  
use "data/endline_data.dta",clear
 
keep if time==2

recode bs_years_since_school (.=0)                                                 
sum $balance

*DEFINE GLOBALS TO CAPTURE OUTCOME EMPLOYMENT FAMILY
 global outc_employment_end work hours_worked written_agreement permanent_work self_employed earnings work_satisfaction
 

capture label var tg_1 "Transport"
capture label var tg_2 "Workshop"
  
capture label var tg_6 "Spillover transport"
capture label var tg_7 "Spillover screening"


gen Transport = ""
gen Screening = ""

gen Spillover_Transport = ""
gen Spollover_Screening = ""

foreach outcome_family in outc_employment_end{

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

foreach x in 6 7{

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
		foreach x in 6 7    {
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

keep Dep_Var Spill_Trans Spill_Screen   ControlMean F N
order Dep_Var Spill_Trans Spill_Screen   ControlMean F N

dataout, save(results/tables/table_a34) tex  replace
insheet using results/tables/table_a34.tex, clear
drop in 1/3
gen n = _n
drop if n == _N
drop n
*replace v1 = "\multicolumn{1}{l}{\emph{Outcome}} & \multicolumn{1}{c}{Transport} &\multicolumn{1}{c}{Job App. Workshop} & \multicolumn{1}{c}{Spillover 1} & \multicolumn{1}{c}{Spillover 2} & \multicolumn{1}{c}{Control Mean} & \multicolumn{1}{c}{F} & \multicolumn{1}{c}{N} \\ \hline \\" in 2
replace v1 = "\multicolumn{1}{l}{\emph{Outcome}} & \multicolumn{1}{c}{Transport Spill} &\multicolumn{1}{c}{Job App. Workshop Spill} & \multicolumn{1}{c}{Control Mean} & \multicolumn{1}{c}{F} & \multicolumn{1}{c}{N} \\ \hline \\" in 2
*gen n = _n
*expand 2 in 2
*sort n
*replace v1 = "&  &  &  &  &  &  &  \\" in 3
outsheet  	v1	using "results/tables/table_a34.tex", noname noquote replace
restore
 }
 
