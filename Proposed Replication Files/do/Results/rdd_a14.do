
use "data/endline_data.dta",replace

keep if time==6

keep if screening_merge==3

global wind = 90

foreach v in monthly_wage   long_tenure {

gen 	`v'_wind 	= `v'
su 		`v'_wind, de
replace `v'_wind 	= r(p$wind) 	if `v'_wind > r(p$wind) & `v'_wind !=.

}

*********************************
* Calculate cutoffs and midpoints
*********************************

forvalues c =50(10)100 {
qui: su agg_score if agg_scoreband == `c'
qui: local band`c'_min = r(min)
qui: local band`c'_max = r(max)
display "band = `c'"
display `band`c'_min'
display `band`c'_max'
}


forvalues c1 =50(10)90 {
local c2 = `c1'+10
local cutoff`c1' = (`band`c1'_max' + `band`c2'_min')/2
}

forvalues c1 =50(10)80 {
local c2 = `c1'+10
local midpoint`c1'_`c2' = (`cutoff`c1'' + `cutoff`c2'')/2
}


scalar avg_half_bin = ((`midpoint80_90' - `midpoint50_60' )/3)/2
display  avg_half_bin
local midpoint40_50 = `cutoff50' - avg_half_bin
local midpoint90_100 = `cutoff90' + avg_half_bin


*****************************
* Generate Bins and normalise
*****************************


* Each bin covers the space from one midpoint to the next midpoint
* The first (last) bin starts at the min (last midpoint) of the score and stretches all the way until the next midpoint (the max).


ge bin = 1 
replace bin = 2 if agg_score < `midpoint60_70' & agg_score >= `midpoint50_60'
replace bin = 3 if agg_score < `midpoint70_80' & agg_score >= `midpoint60_70'
replace bin = 4 if agg_score < `midpoint80_90' & agg_score >= `midpoint70_80'
replace bin = 5 if agg_score >= `midpoint80_90'
tab bin



* Normalise running variable around the cutoff for each bin
* Normalise earnings using the mean and SD of the bin

matrix cutoffs = ( `cutoff50', `cutoff60',  `cutoff70', `cutoff80', `cutoff90')

ge monthly_wage_wind_norm = .
ge earning_wind_norm = .
ge earning_8m_wind_norm = .
ge permanent_work_8m_norm =. 
ge written_agreement_8m_norm =.
ge long_tenure_norm = . 
ge long_tenure_wind_norm = . 

ge runvar = .
forvalues v=1(1)5 {
replace runvar  = agg_score - cutoffs[1,`v'] if bin == `v'
su monthly_wage_wind if bin == `v'
replace  monthly_wage_wind_norm = (monthly_wage_wind - r(mean))/r(sd) if bin == `v'
 
su permanent_work_8m if bin == `v'
replace  permanent_work_8m_norm = (permanent_work_8m - r(mean))/r(sd) if bin == `v'

su written_agreement_8m  if bin == `v'
replace  written_agreement_8m_norm = (written_agreement_8m - r(mean))/r(sd) if bin == `v'

*su long_tenure_cond  if bin == `v'
*replace long_tenure_cond_norm = (long_tenure_cond - r(mean))/r(sd) if bin == `v'


su p2_7  if bin == `v'
replace long_tenure_norm = (p2_7 - r(mean))/r(sd) if bin == `v'

su long_tenure_wind  if bin == `v'
replace long_tenure_wind_norm = (long_tenure_wind - r(mean))/r(sd) if bin == `v'
}


ge cutoff = (runvar  >0)
ge runvar_sq = runvar^2
ge cutoff_runvar = cutoff * runvar
ge cutoff_runvar_sq = cutoff * runvar_sq


 
*************************
* Table with RDD results
*************************

 
  
 local v1 = 1
 local v2 = 2
 local v3 = 3
 
 foreach outc in monthly_wage_wind_norm long_tenure_norm {
	capture drop abs_runvar

	reg `outc' cutoff runvar runvar_sq cutoff_runvar cutoff_runvar_sq [pw=ed_weight], cluster(cluster_id)
		
	local r`v1'_c4 : di %4.3f _b[cutoff]
	local p = 2*(1-normal(abs(_b[cutoff]/_se[cutoff])))
	local p : di %4.3f `p'
	local se : di %4.3f _se[cutoff]
	
	local r`v2'_c4 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
	if `p'<.1 & `p' >=.05 {
	local r`v2'_c4 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
	}
	if `p'<.05 & `p' >=.01 {
	local r`v2'_c4 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
	}
	if `p'<.01  {
	local r`v2'_c4 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
	}
	
count if e(sample) == 1
local r`v3'_c4 = r(N)


rd  `outc' runvar , kernel(rectangular)
local bw = e(w)
ge abs_runvar = abs(runvar)


reg  `outc' cutoff runvar  cutoff_runvar if abs_runvar <= `bw' [pw=ed_weight], cluster(cluster_id)
 	local b0_b`outc' = _b[cutoff]
	local b0_se`outc' = _se[cutoff]
	
	local r`v1'_c1 : di %4.3f _b[cutoff]
	local p = 2*(1-normal(abs(_b[cutoff]/_se[cutoff])))
	local p : di %4.3f `p'
	local se : di %4.3f _se[cutoff]
	
	local r`v2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
	if `p'<.1 & `p' >=.05 {
	local r`v2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
	}
	if `p'<.05 & `p' >=.01 {
	local r`v2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
	}
	if `p'<.01  {
	local r`v2'_c1 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
	}
	
	count if e(sample) == 1
	local r`v3'_c1 = r(N)
	
reg  `outc' cutoff runvar  cutoff_runvar if abs_runvar <= `bw'*0.5 [pw=ed_weight], cluster(cluster_id)
	local r`v1'_c2 : di %4.3f _b[cutoff]
	local p = 2*(1-normal(abs(_b[cutoff]/_se[cutoff])))
	local p : di %4.3f `p'
	local se : di %4.3f _se[cutoff]
	
	local r`v2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
	if `p'<.1 & `p' >=.05 {
	local r`v2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
	}
	if `p'<.05 & `p' >=.01 {
	local r`v2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
	}
	if `p'<.01  {
	local r`v2'_c2 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
	}
	count if e(sample) == 1
	local r`v3'_c2 = r(N)
	

reg  `outc' cutoff runvar  cutoff_runvar if abs_runvar <= `bw'*2 [pw=ed_weight], cluster(cluster_id)
	local r`v1'_c3 : di %4.3f _b[cutoff]
	local p = 2*(1-normal(abs(_b[cutoff]/_se[cutoff])))
	local p : di %4.3f `p'
	local se : di %4.3f _se[cutoff]
	
	local r`v2'_c3 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')}"
	if `p'<.1 & `p' >=.05 {
	local r`v2'_c3 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{*}$}"
	}
	if `p'<.05 & `p' >=.01 {
	local r`v2'_c3 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{**}$}"
	}
	if `p'<.01  {
	local r`v2'_c3 = "\raisebox{.7ex}[0pt]{\scriptsize (`se')$^{***}$}"
	}
	count if e(sample) == 1
	local r`v3'_c3 = r(N)

	local v1 = `v1'+3
	local v2 = `v2'+3
	local v3 = `v3'+3
 }



#delimit ;
file open myfile using "results/tables/table_a14.tex", write replace;
file write myfile   _n "\begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}lccc}"			
_n "\hline"
_n "\mbox{} \\ "
_n "& \multicolumn{3}{c}{Impact on standardised earnings (endline 2)} \\"
_n "\mbox{} \\ "
_n "& \multicolumn{1}{c}{(1)\mbox{\ }} &	\multicolumn{1}{c}{(2)\mbox{\ }} &	\multicolumn{1}{c}{(3)\mbox{\ }}    \\"
_n "\noalign{\smallskip}\hline \noalign{\smallskip}"
_n " Above cut-off & `r1_c1' &	`r1_c2' & `r1_c3'   \\"
_n "  		   & `r2_c1' &	`r2_c2' & `r2_c3'    \\"
_n " \mbox{}\\"
_n " \hline"
_n " \mbox{}\\"
_n " Bandwidth	   & Optimal &	0.5*Optimal & 2*Optimal    \\"
_n "  Obs.		   & `r3_c1' &	`r3_c2' & `r3_c3'  \\"
_n "\mbox{}\\"
_n "\hline"
_n "\mbox{} \\ "
_n "& \multicolumn{3}{c}{Impact on standardised longest tenure} \\"
_n "\mbox{} \\ "
_n "& \multicolumn{1}{c}{(1)\mbox{\ }} &	\multicolumn{1}{c}{(2)\mbox{\ }} &	\multicolumn{1}{c}{(3)\mbox{\ }}    \\"
_n "\noalign{\smallskip}\hline \noalign{\smallskip}"
_n " Above cut-off & `r4_c1' &	`r4_c2' & `r4_c3'   \\"
_n "  		   & `r5_c1' &	`r5_c2' & `r5_c3'    \\"
_n " \mbox{}\\"
_n " \hline"
_n " \mbox{}\\"
_n " Bandwidth	   & Optimal &	0.5*Optimal & 2*Optimal    \\"
_n "  Obs.		   & `r6_c1' &	`r6_c2' & `r6_c3'  \\"
_n "\mbox{}\\"
_n "\hline"
_n "\end{tabular*}%" ;
file close myfile;
#delimit cr


