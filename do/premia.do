clear

window manage forward results


***********
* Get Data
***********

use "data/endline_data.dta"

tab treat_groupind
keep if treat_groupind <= 2 | treat_groupind ==5


global hs_weight 4 
global voc_weight 4/3 
global deg_weight 1 

global ci "1.645"


local bal_vars1 "vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq"
global balance1 "" 
foreach x in  `bal_vars1'{
	global balance1="$balance1" + " bs_" +"`x'"
}


local bal_vars2   "work_freq cent_dist cluster_size"
global balance2 "" 
foreach x in  `bal_vars2'{
	global balance2="$balance2" + " bs_" +"`x'"
}


*****************
* Manipulate Data
*****************


keep if time==6 // This keeps 2018 observations only

ge transport = (treat_groupind ==1)
ge workshop = (treat_groupind ==2)
ge control = (treat_groupind ==5)


replace bs_vocational = ( bs_edu == 1 | bs_edu == 2)

foreach v in degree vocational  {
	ge workshop_I1_`v' = workshop*bs_`v'
	ge transport_I1_`v' = transport*bs_`v'
}

foreach v in experience_perm  {
	ge workshop_I2_`v' = workshop*bs_`v'
	ge transport_I2_`v' = transport*bs_`v'

}



*****************
* Regression
*****************

ivreg2 monthly_wage  transport workshop workshop_I1*   transport_I1* bs_vocational bs_degree bs_earning $balance1 $balance2 [pw=ed_weight] ,    cluster(cluster_id)

test transport + transport_I1_degree =0

global te = _b[transport] + _b[transport_I1_degree]
global mean = _b[bs_degree] + _b[_cons]

	local rz_c1 = `e(N)'
	local r1 = 1
	local r2 = 2
	
	
	foreach v in  workshop bs_vocational bs_degree workshop_I1_vocational workshop_I1_degree {
	
		
		
		local r`r1'_c1 : di %4.3f _b[`v']
		local p = 2*(1-normal(abs(_b[`v']/_se[`v'])))
		local p : di %4.3f `p'
		local se : di %4.3f _se[`v']	
		local r`r2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
		if `p'<.1 & `p' >=.05 {
			local r`r2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
		}
		if `p'<.05 & `p' >=.01 {
			local r`r2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
		}
		if `p'<.01  {
			local r`r2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
		}
	
	local r1 = `r1' +2
	local r2 = `r2' +2
	}
	
	
	
ivreg2 monthly_wage  transport workshop workshop_I2*   transport_I2* bs_experience_perm bs_earning $balance1 $balance2 [pw=ed_weight] , partial($balance1 $balance2) cluster(cluster_id)
	local rz_c2 =  `e(N)'
	local r1 = 1
	local r2 = 2
	
	
	foreach v in  workshop bs_experience_perm workshop_I2_experience_perm  {
		
		local r`r1'_c2 : di %4.3f _b[`v']
		local p = 2*(1-normal(abs(_b[`v']/_se[`v'])))
		local p : di %4.3f `p'
		local se : di %4.3f _se[`v']	
		local r`r2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
		if `p'<.1 & `p' >=.05 {
			local r`r2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
		}
		if `p'<.05 & `p' >=.01 {
			local r`r2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
		}
		if `p'<.01  {
			local r`r2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
		}
		
		local r1 = `r1' +2
		local r2 = `r2' +2
	
	}
	
	
	#delimit ;
file open myfile using "tables/table_a26.tex", write replace;
file write myfile   _n "\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}lcc}"			
_n "\hline"
_n "\mbox{} \\ "
_n "& \multicolumn{2}{c}{Dep var. wage earnings (endline 2)} \\"
_n "\mbox{} \\ "
_n "\hline"
_n "\mbox{} \\ "
_n "& \multicolumn{1}{c}{(1)\mbox{\ }} &	\multicolumn{1}{c}{(2)\mbox{\ }}    \\"
_n "\noalign{\smallskip}\hline \noalign{\smallskip}"
_n " Workshop & `r1_c1' &	`r1_c2'    \\"
_n "  		   & `r2_c1' &	`r2_c2'     \\"
_n " Vocational & `r3_c1' &	   \\"
_n "  		   & `r4_c1' &	    \\"
_n " Degree & `r5_c1' &	   \\"
_n "  		   & `r6_c1' &	     \\"
_n " Workshop * Vocational & `r7_c1' &	   \\"
_n "  		   & `r8_c1' &	     \\"
_n " Workshop * Degree & `r9_c1' &	   \\"
_n "  		   & `r10_c1' &	     \\"
_n " Experience &  &	`r3_c2'   \\"
_n "  		   &  &	  `r4_c2'   \\"
_n " Workshop * Experience &  &	 `r5_c2'  \\"
_n "  		   &  &	 `r6_c2' \\"
_n " \mbox{}\\"
_n " \hline"
_n " \mbox{}\\"
_n "  Obs.		   & `rz_c1' &	`rz_c2'  \\"
_n "\mbox{}\\"
_n "\hline"
_n "\end{tabular*}%" ;
file close myfile;
#delimit cr


