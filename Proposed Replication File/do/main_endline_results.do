*set directory here

cd "/Users/jorgeochoa/OneDrive - Universidad de los andes/PEG/Urban Economics/Final Project/Github Repository/Revised-reproduction-package-for-Abebe-et-al-2021/Revised-reproduction-package-for-Abebe-et-al-2021/Proposed Replication File"

*sets the list of covariate controls to be used in all regressions in the analysis. 
local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}
 
do "utilities/_itt_bothendlines.do"
do "utilities/_itt_oneendline.do"
do "utilities/_itt_het.do"

use "data/endline_data.dta",replace

*immediately drop spillovers 
keep if treat_groupind==1|treat_groupind==2|treat_groupind==5
 
*interaction terms
gen y2015_tg_1 = tg_1*year2015
gen y2015_tg_2 = tg_2*year2015


gen y2018_tg_1 = tg_1*year2018
gen y2018_tg_2 = tg_2*year2018
foreach x in $balance{
gen t_i_`x' = `x'*year2015

}  

 ******************************************   ***********************************************
* This code can be run all at once since the preserve command keeps the dataset in it's original form
* But it can also be run to the end of this segement, and then each chunk of code between headers (horizontal lines) run separately. 
*********************************************************************************************


exit

 ****************************************** TABLE 2 ******************************************
 
itt_maker_jobstime work hours_worked monthly_wage  permanent_work written_agreement work_satisfaction, treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(3) filename(table2) non cp

 *********************************************************************************************

 
 
 ****************************************** TABLE 3 ************************************************
 preserve
keep if time ==6
itt_maker_jobs  monthly_wage_cond longesttenure_cond p2_7 uses_skills promote    , treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(3) filename(table3)
 restore
****************************************************************************************************
  
*** NOTE: The final rows for Tables 4, 5, and 6 are generated separately in the file "endogenous_stratification.do"

******************************************* TABLE 4 ************************************************
preserve
keep if time ==6																						
***** 	TO DO: WE HAVE TO APPEND THE FINAL ROW MANUALLY
itt_maker_het tertiary_ed  male bs_high_intense bs_experience_perm close_centre, outcome(monthly_wage) treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(1) filename(table4) non  cp
restore
****************************************************************************************************


 ****************************************** TABLE 5 and 6 ****************************************** 
 
local indi = 5
foreach var of varlist permanent_work written_agreement{
	
	dis "*****************************************************"
	dis "Table `indi'"
	dis "*****************************************************"
	
	preserve 
	keep if time==2
	
	itt_maker_het tertiary_ed  male bs_high_intense bs_experience_perm close_centre, outcome(`var') treat1(tg_1) treat2(tg_2) covariates($balance) ///
																					 decimals(3) filename(table`indi') non  cp
	
	restore
	
	local indi = `indi' + 1
}

***************************************************************************************************

* APPENDICES **************************************************************************************
  
  
****************************************** TABLE A.9 **********************************************
preserve 
global Covar3 "bs_female bs_respondent_age bs_married bs_live_parents bs_amhara bs_oromo bs_migrant_birth bs_degree bs_years_since_school  bs_work bs_search  bs_work_freq  bs_search_freq bs_work_wage_6months bs_search_6months bs_experience_perm"
keep if time==2
gen beyond6km = bs_cent_dist>5.8 if bs_cent_dist!=.

ge edu2_1 = ( bs_education==0)
ge edu2_2= ( bs_education==1|bs_education==2)
ge edu2_3 =  ( bs_education==3)

rename edu2_3 Degree

recode i2_1 (2=0)
rename i2_1 transport_takeup_self
recode i3_1 (2=0)
rename i3_1 workshop_takeup_self
 
label var bs_female "Female"
label var bs_respondent_age "Age"
 
reg transport_takeup_self $Covar3 if treat_groupind == 1, cluster(cluster_id)
est2vec table_a9, replace name(Transport) vars($Covar3 _cons) e(N F)
local p1 = 1- F(`e(df_m)' ,`e(df_r)' ,`e(F)')
local p1 : di %4.3f `p1'
reg workshop_takeup_self $Covar3 if treat_groupind == 2, cluster(cluster_id)
est2vec table_a9, addto(table_a9) name(Workshop) 
local p2 = 1- F(`e(df_m)' ,`e(df_r)' ,`e(F)')
local p2 : di %4.3f `p2'

est2tex table_a9, replace path(tables) label  preserve  digit(3) mark(stars)  fancy 

display "p value F-test transport take-up " `p1'
display "p value F-test workshop take-up " `p2'
restore 

***************************************************************************************************

  
****************************************** TABLE A.10 *********************************************

itt_maker_jobstime monthly_wage ln_monthly_wage  monthly_wage_w99 monthly_wage_w95 monthly_wage_w90   , treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(3) filename(table_a10) non cp

***************************************************************************************************

****************************************** TABLE A.11 *********************************************

itt_maker_jobstime monthly_wage earnings      additive_wages_winsor , treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(3) filename(table_a11) non cp

***************************************************************************************************

****************************************** TABLE A.12 &  A.13 *************************************
preserve 
keep if time==6

****************************************** TABLE A.12 *********************************************
forvalues x =0.4(0.05)1{
qreg monthly_wage tg_1 tg_2  bs_monthly_wage  $balance  if time==6  [pw=ed_weight] , quantile(`x')

}
***************************************************************************************************

****************************************** TABLE A.13 *********************************************

forvalues x =0.4(0.05)1{
qreg earnings tg_1 tg_2  bs_earnings  $balance  if time==6  [pw=ed_weight] , quantile(`x')
}
restore

***************************************************************************************************

****************************************** TABLE A.14 *********************************************

*THIS TABLE IS PRODUCED IN THE FILE rdd_a14.do

***************************************************************************************************

****************************************** TABLES A.15-A.23   *************************************
 
 *THESE TABLES ARE PRODUCED IN THE FILE  complete_family_tables_a15_a23.do

***************************************************************************************************

****************************************** TABLE A.24 DEMEDIATION DO FILE *************************

****************************************** TABLE A.25   *******************************************

preserve 
keep if time ==6
itt_maker_het tertiary_ed  male bs_high_intense bs_experience_perm close_centre    bornaddis cvcertificate present_bias   bs_network_size , outcome(monthly_wage) treat1(tg_1) treat2(tg_2) ///
covariates($balance) decimals(1) filename(table_a25) non  cp
restore
 
***************************************************************************************************
 
****************************************** FIGURES 3 AND A.3   ************************************
preserve
keep if time==6

su monthly_wage, d
drop if monthly_wage >= `r(p99)'
ge cumul_earn =.
foreach v in 1 2 5 {
 cumul monthly_wage if treat_groupind ==`v' [aw=ed_weight] , gen(cumul_earn`v') eq
replace cumul_earn = cumul_earn`v' if treat_groupind ==`v'
}

*cumulative earnings plots (copied from Stefano)
twoway (line cumul_earn5 monthly_wage if treat_groupind ==5, sort) (line cumul_earn2 monthly_wage if treat_groupind ==2, sort) , ///
legend (label(1 "Control") label(2 "Workshop")) ///
 ytitle("Cumulative percent")  ///
       graphr(fc(white) lc(white)) graphregion(margin(l+5 r+5)) ///
        xtitle("Earnings", margin(medium)) 
	*graph save  "01 Paper/Figures/Second_endline/Dominance.gph" , replace
	graph export "figures/figure3.png", replace width(1600) height(1200)

	
	
twoway (line cumul_earn5 monthly_wage if treat_groupind ==5, sort) (line cumul_earn1 monthly_wage if treat_groupind ==1, sort) , ///
legend (label(1 "Control") label(2 "Transport")) ///
 ytitle("Cumulative percent")  ///
       graphr(fc(white) lc(white)) graphregion(margin(l+5 r+5)) ///
        xtitle("Earnings", margin(medium)) 
 *  graph save  "figures/Figures/Second_endline/Dominance_transport.gph" , replace
	graph export "figures/figure_a3.png", replace width(1600) height(1200)
	
restore

***************************************************************************************************

local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}
 
 ******************** FIGURE A.8 ********************

gen control= treat_groupind==5


*here we rank occupations by their 2018 average earnings and impute that back to 2015 occupational categories.
gen occ_2015a = occupation if time==2
egen occ_2015 = max(occ_2015a),by(r_id)
replace occ_2015=. if time!=6
egen mean_earnings= mean(monthly_wage), by(occ_2015)
replace p1_4 = occupation if p1_4 ==.
replace occ_2015 = occupation if occ_2015 ==.
egen mean_earnings_all=  max(mean_earnings), by(occ_2015)

egen all_n1 = count(p1_4) ,by(p1_4)

foreach x in 1 2{
preserve 
keep if time ==2
keep p1_4  control mean_earnings_all time tg_1 tg_2 all_n1
keep if control==1|tg_`x'==1
gen n = 1

collapse (first) mean_earnings all_n1 (count) n   , by( p1_4 control )
drop if p1_4==.

egen all_n = total(n) ,by(p1_4)

egen all_treat = total(n) , by(control)
gen double n_frac= n/all_treat
drop if all_n1<6
drop if all_n1<20
*drop "other"
drop if p1_4==37

keep n_frac p1_4  all_n   mean_earnings   control
reshape wide n_frac , i(p1_4) j(control)

recode n_frac0 n_frac1   (.=0)

sort n_frac1
label variable n_frac0 "Transport"
if `x'==2{
label variable n_frac0 "Workshop"

}
label variable n_frac1 "Control"
 

graph hbar (asis) n_frac0 n_frac1  , ///
over(p1_4, sort(mean_earnings) descending)   ///
 plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white)) legend( cols(3)  position(6)) scheme(538w)
	graph export "figures/figure_a8_`x'.png", replace width(1600) height(1200)

restore
}

***************************************************************************************************

******************** FIGURES A.4 AND A.5 ********************

*append recall data which constructs employment rates by year for the intervening years before the first and second endline. 
append using  data/recall.dta
gen timeplot =. 
gen treatment = . 
gen depvar =""
gen y_cm =. 
gen y_b =. 
gen y_cit =. 
gen y_cib =. 

local j = 1
local counter = 1 

foreach outcome in permanent_work work      {
 forvalues x =1/6{
forvalues t=1/2{
replace treatment = `t' in `j'

sum `outcome' if treat_groupind==5&time==`x' [aw=ed_weight]
replace y_cm = r(mean) in `j' 

ivreg2 `outcome' tg_1 tg_2 bs_`outcome'  $balance  if time==`x' [pw=ed_weight], partial($balance) cluster(cluster_id)
replace y_b  = _b[tg_`t'] +y_cm    in `j' 
replace y_cit = y_b +1.645*_se[tg_`t']  in `j' 
replace y_cib =  y_b -1.645*_se[tg_`t']  in `j' 

replace timeplot = `x' in `j'
replace depvar = "`outcome'" in `j'

local j = `j'+1
}
local j = `j'+1
}
}

gen no = _n 
keep if _n<`j' 


drop if timeplot==1
replace no = no -3

local bar_details color(black)  blw(*.4)  barwidth(1)
local cap_details fcolor(black) lcolor(black) 

preserve 
keep if depvar=="permanent_work"
twoway (bar y_cm no,  bcolor(gs14) fintensity(inten100) `bar_details' ylabel(0(0.1)0.4 ) ymtick(0.1(0.1)0.4 ) ) ///
(scatter y_b  no if treatment==1  , yaxis(1)  mcolor(blue) msymbol(diamond) ) ///
(rcap y_cit y_cib no if treatment==1 , yaxis(1)  `cap_details' )    /// 
(scatter y_b  no if treatment==2  , yaxis(1)  mcolor(red) msymbol(circle) ) ///
(rcap y_cit y_cib no if treatment==2 , yaxis(1)  `cap_details' )  ///   
, xlabel(1.3 "Sept 2015" 4.2 "Year to Sept 2016" 7.5 "Year to Sept 2017" 10.6 "Year to May 2018" 13.5 "May 2018", noticks labsize(small) ) ///
xtitle("Time Period") ytitle("Proportion",  axis(1))   plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white)) ///
title("Effect on permanent work over time", position(12) ring(0) alignment(top) height(3) size(medium)) ///
legend(order(1 "Control Mean" 2 "Transport" 4 "Workshop" 5 "90% CI for treatment effect") ///
cols(4) symxsize(6) rowgap(2) size(small) pos(6)) 
graph export "figures/figure_a5.pdf", as(pdf) replace

restore 

preserve
keep if depvar=="work"
replace no = no-18

local bar_details color(black)  blw(*.4)  barwidth(1)
local cap_details fcolor(black) lcolor(black) 

twoway (bar y_cm no,  bcolor(gs14) fintensity(inten100) `bar_details' ylabel(0(0.1)1 ) ymtick(0.1(0.1)1 ) ) ///
(scatter y_b  no if treatment==1  , yaxis(1)  mcolor(blue) msymbol(diamond) ) ///
(rcap y_cit y_cib no if treatment==1 , yaxis(1)  `cap_details' )    /// 
(scatter y_b  no if treatment==2  , yaxis(1)  mcolor(red) msymbol(circle) ) ///
(rcap y_cit y_cib no if treatment==2 , yaxis(1)  `cap_details' )  ///   
, xlabel(1.3 "Sept 2015" 4.2 "Year to Sept 2016" 7.5 "Year to Sept 2017" 10.6 "Year to May 2018" 13.5 "May 2018", noticks labsize(small) ) ///
xtitle("Time Period") ytitle("Proportion",  axis(1))   plotregion(fcolor(white) lcolor(white)) graphregion(fcolor(white) lcolor(white)) ///
title("Effect on any work over time", position(12) ring(0) alignment(top) height(3) size(medium)) ///
legend(order(1 "Control Mean" 2 "Transport" 4 "Workshop" 5 "90% CI for treatment effect") ///
cols(4) symxsize(6) rowgap(2) size(small) pos(6)) 

graph export "figures/figure_a4.pdf", as(pdf) replace
restore
 
 
 ** END OF FILE *** 
 
 exit
