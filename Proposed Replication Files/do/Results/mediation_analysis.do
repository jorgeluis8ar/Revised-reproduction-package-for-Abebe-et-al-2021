
window manage forward results

***********
* Get Data
***********
use "data/endline_data.dta", replace


keep if treat_groupind==1|treat_groupind==2|treat_groupind==5

ge transport = (treat_groupind ==1)
ge workshop = (treat_groupind ==2)
ge control = (treat_groupind ==5)
ge transport_t = transport
ge workshop_t = workshop

tab  treat_groupind

global var "work monthly_wage permanent_work written_agreement long_tenure_uncond uses_skills"
keep $var hours_worked  r_id time transport workshop control bs_* ed_weight cluster_id transport_t workshop_t 

*jobtenure
reshape wide $var hours_worked , i(r_id) j(time)

global ci "1.645"

local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}
 

*****************
* Manipulate Data
*****************

** Demediation **
global hs_weight 4 
global voc_weight 4/3 
global deg_weight 1 


global rN = "3"
global rN2 = $rN +1

local var1 "permanent_work2"
local var2 "long_tenure_uncond6"
local var3 "permanent_work2 long_tenure_uncond6"



*********************** REPLICATE TABLE A.24  ***********************
*replace jobtenure6 = jobtenure6/12
**SF: I added this Mincerian regressions. 

ivreg2 monthly_wage6 work2 hours_worked2 monthly_wage2 permanent_work2  [pw=ed_weight] if control==1, cluster(cluster_id)
outreg2  using "results/tables/table_a24.tex" , tex replace
ivreg2 monthly_wage6 work2 hours_worked2 monthly_wage2 permanent_work2 long_tenure_uncond6   [pw=ed_weight] if control==1, cluster(cluster_id)
outreg2  using "results/tables/table_a24.tex" , tex append
ivreg2 monthly_wage6 work2 hours_worked2 monthly_wage2 permanent_work2 $balance [pw=ed_weight] if control==1, partial($balance) cluster(cluster_id)
outreg2  using "results/tables/table_a24.tex" , tex append
ivreg2 monthly_wage6 work2 hours_worked2 monthly_wage2 permanent_work2 long_tenure_uncond6 $balance [pw=ed_weight] if control==1 , partial($balance) cluster(cluster_id)
outreg2  using "results/tables/table_a24.tex" , tex append

/*
	
	The previous regression uses the command for Instrumental variables, but as it does no defines intruments,
	the second first stage is not estimated and thus the command estimates a simple ols linear regression. So why 
	do the author do that? The reason is because they are using the option partial. What this option does is that
	it uses the Frisch-Waugh-Lovell (FWL) theorem and partials out the coefficients of the regression and the ones
	that are kept still have the same standard error and point estimate.
	
	Finally, the regression estimates the correlates of the 2018 wage earnings. The results show that having had a permanent
	work by 2015 increases wage earning by sround 900 $. This effects is rather statistical significant.

*/
  
ivreg2 monthly_wage6   transport workshop bs_monthly_wage $balance  [pw=ed_weight]  , partial($balance ) cluster(cluster_id)
matrix all = [_b[workshop], _b[workshop] + $ci*_se[workshop], _b[workshop] - $ci*_se[workshop]]
global b0 = _b[workshop]


/*
The following ckunk of code provide results necesary to produce figure 4.

What the code does is resume as follows>

1. In previous exercises or in table a.24 we generated marginal effects of some covariates on the 2018 wage earnings.
   This estimates were stored in a matrix called all.
2. The following exercise is to predict the monthly wage for those individual in the treatment group as if they had been
   assigned to the control group. This is done by estimating a regression and then replacinng the treatment status to
   control (replace treatment = 0). Then we can predict what they 2018 wage earning would have been. The measure is not 
   exact, but still is a good measure of a counterfactual.
3. Then, this predicted wage earning is regressed by the same specification were the matrix all was created.
4. Then, the authors proceed to calculate a linear combination of the estimates to calculate the marginal effect 
   of the job application workshop in wage earnings. See figure 4 of the paper.
5. All regressions use the Frisch-Waugh-Lovell (FWL) theorem to partial out covariates.

*/
dis "${rN}"
forvalues s = 1(1)$rN {

	preserve
		
	foreach v in  `var`s''{
		ge transport_I_`v' = transport*`v'
		ge workshop_I_`v' = workshop*`v'
	}


	reg monthly_wage6 `var`s''  $balance  transport workshop transport_I_* workshop_I_*  

	replace transport =0
	replace workshop =0
	predict monthly_wage2res , res

	replace transport = transport_t
	replace workshop = workshop_t

	ivreg2 monthly_wage2res transport workshop bs_monthly_wage $balance  [pw=ed_weight]  , partial($balance ) cluster(cluster_id)
	matrix temp = [_b[workshop], _b[workshop] + $ci*_se[workshop], _b[workshop] - $ci*_se[workshop]]
	matrix all =( temp \ all)

		
	restore
}


clear
 svmat all , names(var)
 ge row = _n
 
 
 forvalues v =1(1)$rN{
 su var1 if row == `v'
 local c`v' = r(mean)
 local d`v' = (1-`c`v''/ $b0)*100
 local d`v' : di %4.0f `d`v''
 local d`v' = "`d`v''"

 }
 
 display $b0
 ge var4 = (1-var1 / $b0)

twoway (scatter row  var1  if row < $rN2  , mc(black) ) ///
		(scatter row  var1  if row == $rN2  , mc(black)  msymbol(Oh)) ///
		(rspike var2 var3 row if row == $rN2, lc(black) lp(dash) hor  ) ///
       (rspike var2 var3 row if row < $rN2, lc(black) hor  ) , ///
      ytitle("")  ylabel(1(1)4, grid gst(foreground) glw(medthick))  ///
       yscale(range(0.5 5))  xscale(range(-100 500))   ///
      graphr(fc(white) lc(white)) graphregion(margin(l+5 r+5)) xtitle("Impact on endline 2 earnings", margin(medium)) ///
      legend(off) xline(0, lc(black) lp(dash)) ///
      ylabel(4 "Original treatment effect (endline 2)"  3 "Permanent work (uncond., endline 1)" 2 "Longest tenure (uncond., endline 1)" 1 "Permanent work + longest tenure" , angle(horizontal) ) ///
       text(2.8 `c3'  "(`d3'%)", place(s)) ///
      text(1.8 `c2'  "(`d2'%)", place(s)) text(0.8 `c1'  "(`d1'%)", place(s) ) ///
      text(2.8 -1160  "Mediator fixed:", place(s) orientation(vertical)) ///
      text(3.5 -1100  "|_______________________|", place(s) orientation(rvertical))
      
    	graph export "results/figures/figure4.png", replace width(1600) height(1200)


exit



