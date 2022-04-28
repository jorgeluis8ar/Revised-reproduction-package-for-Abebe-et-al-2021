
cd "/Users/jorgeochoa/OneDrive - Universidad de los andes/PEG/Urban Economics/Final Project/Github Repository/Revised-reproduction-package-for-Abebe-et-al-2021/Revised-reproduction-package-for-Abebe-et-al-2021/Original Replication Files"

do "utilities/_itt_onetreat.do"
 

local bal_vars search_freq degree work search female respondent_age  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}
 

use "data/attrition_bounds.dta",clear 

*Predictors of attrition (Table A.8)

/*
The following regressions provide evidence on attrition of individuals. The key variables of
interest are the dummy variables of treatment branch. The first and third regressions are estimated
using the following equation:

attrition_ic = β_0 + β_1 transport_ic + β_2 Workshop_ic +  μ_{ic}

Regressions 2 and 4 are estimated using a set of controls

attrition_ic = β_0 + β_1 transport_ic + β_2 Workshop_ic +  δ x X_{ic0} + μ_{ic}

The elements of the regression are:
	1. Dependent variable: Dummy variable identifying if individual i takes the follow up survey
	2. Regressors: Defined as the dummy treatment variables for both brances and a vector of controls
	3. Standard cluster errors: As the randomization of the sample and treatments was done in two steps.
	   Firstly geographical zones and secondly within zones teatment randomization, the standar errors 
	4. Subsample: Regressions 1 and 2 assess the predictors of attrition in the first endline, while equations
	    3 and 4 assess the predictors of attrition in the second endline.
*/
reg  attritted tg_1 tg_2            if time==2 ,cluster(cluster_id)
reg  attritted tg_1 tg_2  $balance  if time==2 ,cluster(cluster_id)
reg  attritted tg_1 tg_2            if time==6 ,cluster(cluster_id)
reg  attritted tg_1 tg_2  $balance  if time==6 ,cluster(cluster_id)

keep if time ==6 

* 1) EARNINGS UPPER BOUND FOR 2018  (Table A.28)
reg monthly_wage $balance  if control==1
predict predict_wage 
predict pred_wage_se ,stdp

gen monthly_wage_pred = monthly_wage
replace monthly_wage_pred = predict_wage if monthly_wage==.
  
gen monthly_wage_pred_se25 = monthly_wage
replace monthly_wage_pred_se25 = predict_wage-(0.25*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se25 = predict_wage+(0.25*pred_wage_se) if monthly_wage==.&tg_2==1

gen monthly_wage_pred_se50 = monthly_wage
replace monthly_wage_pred_se50 = predict_wage-(0.50*pred_wage_se) if monthly_wage==.&control==1
replace monthly_wage_pred_se50 = predict_wage+(0.50*pred_wage_se) if monthly_wage==.&tg_2==1

gen monthly_wage_imp25_gened = monthly_wage
gen monthly_wage_imp50_gened =monthly_wage

forvalues y =0/1{
forvalues x =0/1{
sum monthly_wage_imp25_gened if control == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp25_gened = r(mean)-0.25*r(sd) if monthly_wage==.&control == 1  &bs_female==`x'&hs==`y'
sum monthly_wage_imp25_gened if tg_2 == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp25_gened = r(mean)+0.25*r(sd) if monthly_wage==.&tg_2 == 1 &bs_female==`x'&hs==`y'

sum monthly_wage_imp50_gened if control == 1  &bs_female==`x'&hs==`y'
replace monthly_wage_imp50_gened = r(mean)-0.5*r(sd) if monthly_wage==.&control == 1  &bs_female==`x'&hs==`y'
sum monthly_wage_imp50_gened if tg_2 == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp50_gened = r(mean)+0.5*r(sd) if monthly_wage==.&tg_2 == 1 &bs_female==`x'&hs==`y'
}
}

gen monthly_wage_imp95 = monthly_wage
gen monthly_wage_manski = monthly_wage

sum monthly_wage_imp95 if control == 1 ,d
replace monthly_wage_imp95 = r(p5) if monthly_wage==.&control == 1 
replace monthly_wage_manski = r(min) if monthly_wage==.&control == 1 

sum monthly_wage_imp95 if tg_2 == 1,d
replace monthly_wage_imp95 =   r(p95)  if monthly_wage==.&tg_2 == 1 
replace monthly_wage_manski =  r(max) if monthly_wage==.&tg_2 == 1 

recode bs_education (2/3=2) (4=3)
keep if control==1|tg_2==1

label var monthly_wage_pred "Predicted earnings" 
label var monthly_wage_pred_se25 "Predicted earnings +/- 0.25 SDs" 
label var monthly_wage_pred_se50 "Predicted earnings +/- 0.5 SDs" 
label var monthly_wage_imp25_gened "Mean control earnings +/- 0.25 SDs" 
label var monthly_wage_imp50_gened "Mean control earnings +/- 0.5 SDs" 
label var monthly_wage_imp95 "95th / 5th percentile" 
label var monthly_wage_manski "Max/min" 

  itt_maker_onetreat  monthly_wage_pred  monthly_wage_pred_se25 monthly_wage_pred_se50  ///
 monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95   monthly_wage_manski, treat(tg_2)  ///
covariates($balance bs_earnings) decimals(1) filename(table_a28)

 
* 2) EARNINGS LOWER BOUND FOR 2018 (Table A.27)
use "data/attrition_bounds.dta",clear
keep if time==6
reg monthly_wage $balance  if control==1
predict predict_wage 
predict pred_wage_se ,stdp

gen monthly_wage_pred = monthly_wage
replace monthly_wage_pred = predict_wage if monthly_wage==.
  
gen monthly_wage_pred_se25 = monthly_wage
replace monthly_wage_pred_se25 = predict_wage-(0.25*pred_wage_se) if monthly_wage==.&tg_2==1
replace monthly_wage_pred_se25 = predict_wage+(0.25*pred_wage_se) if monthly_wage==.&control==1

gen monthly_wage_pred_se50 = monthly_wage
replace monthly_wage_pred_se50 = predict_wage-(0.50*pred_wage_se) if monthly_wage==.&tg_2==1
replace monthly_wage_pred_se50 = predict_wage+(0.50*pred_wage_se) if monthly_wage==.&control==1

gen monthly_wage_imp25_gened = monthly_wage
gen monthly_wage_imp50_gened =monthly_wage

forvalues y =0/1{
forvalues x =0/1{
sum monthly_wage_imp25_gened if control == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp25_gened = r(mean)+0.25*r(sd) if monthly_wage==.&control == 1  &bs_female==`x'&hs==`y'
sum monthly_wage_imp25_gened if tg_2 == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp25_gened = r(mean)-0.25*r(sd) if monthly_wage==.&tg_2 == 1 &bs_female==`x'&hs==`y'

sum monthly_wage_imp50_gened if control == 1  &bs_female==`x'&hs==`y'
replace monthly_wage_imp50_gened = r(mean)+0.5*r(sd) if monthly_wage==.&control == 1  &bs_female==`x'&hs==`y'
sum monthly_wage_imp50_gened if tg_2 == 1 &bs_female==`x'&hs==`y'
replace monthly_wage_imp50_gened = r(mean)-0.5*r(sd) if monthly_wage==.&tg_2 == 1 &bs_female==`x'&hs==`y'
}
}

 
gen monthly_wage_imp95 = monthly_wage
gen monthly_wage_manski = monthly_wage

sum monthly_wage_imp95 if tg_2 == 1 ,d
replace monthly_wage_imp95 = r(p5) if monthly_wage==.&tg_2 == 1 
replace monthly_wage_manski = r(min) if monthly_wage==.&tg_2 == 1 

sum monthly_wage_imp95 if control == 1,d
replace monthly_wage_imp95 =   r(p95)  if monthly_wage==.&control == 1 
replace monthly_wage_manski =  r(max) if monthly_wage==.&control == 1 

recode bs_education (2/3=2) (4=3)
keep if control==1|tg_2==1

label var monthly_wage_pred "Predicted earnings" 
label var monthly_wage_pred_se25 "Predicted earnings +/- 0.25 SDs" 
label var monthly_wage_pred_se50 "Predicted earnings +/- 0.5 SDs" 
label var monthly_wage_imp25_gened "Mean control earnings +/- 0.25 SDs" 
label var monthly_wage_imp50_gened "Mean control earnings +/- 0.5 SDs" 
label var monthly_wage_imp95 "95th / 5th percentile" 
 label var monthly_wage_manski "Max/min" 

itt_maker_onetreat  monthly_wage_pred  monthly_wage_pred_se25 monthly_wage_pred_se50  ///
 monthly_wage_imp25_gened monthly_wage_imp50_gened monthly_wage_imp95   monthly_wage_manski, treat(tg_2)  ///
covariates($balance bs_earnings) decimals(1) filename(table_a27)



***** 2015 BOUNDS (Tables A.29-A.33)
use "data/attrition_bounds.dta",clear
keep if time==2

*Table A.33: Lee Bounds on binary variables in 2015.
leebounds permanent_work tg_2   [pw=ed_weight] if control == 1 | tg_2 == 1  ,    cieffect 
leebounds written_agreement tg_2   [pw=ed_weight] if control == 1 | tg_2 == 1  ,    cieffect 

leebounds permanent_work tg_1   [pw=ed_weight] if control == 1 | tg_1 == 1  ,    cieffect 
leebounds written_agreement tg_1   [pw=ed_weight] if control == 1 | tg_1 == 1  ,    cieffect 


local outcome1 written_agreement
local outcome1 permanent_work

sum permanent_work tg_1 ed_weight control

local treat tg_1
*karlan-vidia method
local tab 29 
foreach treat in tg_2 tg_1 {
foreach outcome1 in permanent_work written_agreement {

preserve
reg `outcome1' $balance  if control==1
predict pred_var 
predict pred_var_se ,stdp


gen `outcome1'_pred = `outcome1'
replace `outcome1'_pred = pred_var if `outcome1'==.

gen `outcome1'_pred_se25 = `outcome1'
replace `outcome1'_pred_se25 = pred_var+(0.25*pred_var_se) if `outcome1'==.&control==1
replace `outcome1'_pred_se25 = pred_var-(0.25*pred_var_se) if `outcome1'==.&`treat'==1

gen `outcome1'_pred_se50 = `outcome1'
replace `outcome1'_pred_se50 = pred_var+(0.50*pred_var_se) if `outcome1'==.&control==1
replace `outcome1'_pred_se50 = pred_var-(0.50*pred_var_se) if `outcome1'==.&`treat'==1


*impute control and treatment 
gen `outcome1'_imp25 = `outcome1'
gen `outcome1'_imp50 =`outcome1'

gen `outcome1'_imp25_hs = `outcome1'
gen `outcome1'_imp50_hs =`outcome1'

gen `outcome1'_imp25_ed = `outcome1'
gen `outcome1'_imp50_ed =`outcome1'

gen `outcome1'_imp25_gen = `outcome1'
gen `outcome1'_imp50_gen =`outcome1'

gen `outcome1'_imp25_gened = `outcome1'
gen `outcome1'_imp50_gened =`outcome1'


sum `outcome1',d
*replace `outcome1'_imp25 = r(p95) if `outcome1'>r(p95)&`outcome1'!=.
*replace `outcome1'_imp50 = r(p95) if `outcome1'>r(p95)&`outcome1'!=.

recode bs_education (2/3=2) (4=3)
 

sum `outcome1'_imp25 if control == 1  
replace `outcome1'_imp25 = r(mean)+0.25*r(sd) if `outcome1'==.&control == 1  
sum `outcome1'_imp25 if `treat' == 1  
replace `outcome1'_imp25 = r(mean)-0.25*r(sd) if `outcome1'==.&`treat' == 1 

sum `outcome1'_imp50 if control == 1  
replace `outcome1'_imp50 = r(mean)+0.5*r(sd) if `outcome1'==.&control == 1 
sum `outcome1'_imp50 if `treat' == 1  
replace `outcome1'_imp50 = r(mean)-0.5*r(sd) if `outcome1'==.&`treat' == 1 


forvalues x =0/1{
sum `outcome1'_imp25_hs if control == 1 &hs==`x'
replace `outcome1'_imp25_hs = r(mean)+0.25*r(sd) if `outcome1'==.&control == 1  &hs==`x'
sum `outcome1'_imp25_hs if `treat' == 1 &hs==`x'
replace `outcome1'_imp25_hs = r(mean)-0.25*r(sd) if `outcome1'==.&`treat' == 1 &hs==`x'

sum `outcome1'_imp50_hs if control == 1  &hs==`x'
replace `outcome1'_imp50_hs = r(mean)+0.5*r(sd) if `outcome1'==.&control == 1  &hs==`x'
sum `outcome1'_imp50_hs if `treat' == 1 &bs_education==`x'
replace `outcome1'_imp50_hs = r(mean)-0.5*r(sd) if `outcome1'==.&`treat' == 1 &hs==`x'
}

forvalues x =0/2{
sum `outcome1'_imp25_ed if control == 1 &bs_education==`x'
replace `outcome1'_imp25_ed = r(mean)+0.25*r(sd) if `outcome1'==.&control == 1  &bs_education==`x'
sum `outcome1'_imp25_ed if `treat' == 1 &bs_education==`x'
replace `outcome1'_imp25_ed = r(mean)-0.25*r(sd) if `outcome1'==.&`treat' == 1 &bs_education==`x'

sum `outcome1'_imp50_ed if control == 1  &bs_education==`x'
replace `outcome1'_imp50_ed = r(mean)+0.5*r(sd) if `outcome1'==.&control == 1  &bs_education==`x'
sum `outcome1'_imp50_ed if `treat' == 1 &bs_education==`x'
replace `outcome1'_imp50_ed = r(mean)-0.5*r(sd) if `outcome1'==.&`treat' == 1 &bs_education==`x'
}


forvalues x =0/1{
sum `outcome1'_imp25_gen if control == 1 &bs_female==`x'
replace `outcome1'_imp25_gen = r(mean)+0.25*r(sd) if `outcome1'==.&control == 1  &bs_female==`x'
sum `outcome1'_imp25_gen if `treat' == 1 &bs_female==`x'
replace `outcome1'_imp25_gen = r(mean)-0.25*r(sd) if `outcome1'==.&`treat' == 1 &bs_female==`x'

sum `outcome1'_imp50_gen if control == 1  &bs_female==`x'
replace `outcome1'_imp50_gen = r(mean)+0.5*r(sd) if `outcome1'==.&control == 1  &bs_female==`x'
sum `outcome1'_imp50_gen if `treat' == 1 &bs_female==`x'
replace `outcome1'_imp50_gen = r(mean)-0.5*r(sd) if `outcome1'==.&`treat' == 1 &bs_female==`x'
}


forvalues y =0/1{
forvalues x =0/1{
sum `outcome1'_imp25_gened if control == 1 &bs_female==`x'&hs==`y'
replace `outcome1'_imp25_gened = r(mean)+0.25*r(sd) if `outcome1'==.&control == 1  &bs_female==`x'&hs==`y'
sum `outcome1'_imp25_gened if `treat' == 1 &bs_female==`x'&hs==`y'
replace `outcome1'_imp25_gened = r(mean)-0.25*r(sd) if `outcome1'==.&`treat' == 1 &bs_female==`x'&hs==`y'

sum `outcome1'_imp50_gened if control == 1  &bs_female==`x'&hs==`y'
replace `outcome1'_imp50_gened = r(mean)+0.5*r(sd) if `outcome1'==.&control == 1  &bs_female==`x'&hs==`y'
sum `outcome1'_imp50_gened if `treat' == 1 &bs_female==`x'&hs==`y'
replace `outcome1'_imp50_gened = r(mean)-0.5*r(sd) if `outcome1'==.&`treat' == 1 &bs_female==`x'&hs==`y'
}
}

 
keep if control==1|`treat'==1


label var `outcome1'_pred "Imputed `outcome1'" 
label var `outcome1'_pred_se25 "Imputed `outcome1' +/- 0.25 SDs" 
label var `outcome1'_pred_se50 "Imputed `outcome1' +/- 0.5 SDs" 
label var `outcome1'_imp25_gened "Mean control `outcome1' +/- 0.25 SDs" 
label var `outcome1'_imp50_gened "Mean control `outcome1' +/- 0.5 SDs" 

 reg `outcome1'_pred `treat'  $balance [pw=ed_weight] if control == 1 | `treat' == 1 
 reg `outcome1'_pred_se25 `treat'  $balance [pw=ed_weight] if control == 1 | `treat' == 1 
 
 
 sum  `outcome1'_pred  `outcome1'_pred_se25 `outcome1'_pred_se50  ///
 `outcome1'_imp25_gened `outcome1'_imp50_gened
 
 
itt_maker_onetreat  `outcome1'_pred  `outcome1'_pred_se25 `outcome1'_pred_se50  ///
 `outcome1'_imp25_gened `outcome1'_imp50_gened , treat(`treat')  ///
covariates($balance bs_earnings) decimals(3) filename(table_a`tab')
local tab = `tab' +1

restore
}
}
