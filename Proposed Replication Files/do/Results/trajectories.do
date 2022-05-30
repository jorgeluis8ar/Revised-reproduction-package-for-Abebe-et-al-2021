************************************************************************************

		* This file produces figures showing the trajectory of impacts over time using the high-frequency phone surveys

************************************************************************************


use "data/phone_panel.dta",clear
*define programs that will impose quadratic constraints on the trend line
do "do/utilities/_DefineQuadraticConstraintProgram"
do "do/utilities/_DefineQuadraticConstraintProgram_monthly"

local bal_vars vocational work search post_secondary female  migrant_birth amhara oromo work_wage_6months  married  live_parents  experience_perm   search_6months respondent_age years_since_school search_freq work_freq cent_dist cluster_size
global balance "" 
foreach x in  `bal_vars'{
	global balance="$balance" + " bs_" +"`x'"
}
 

*drop the first week of data. we have few data points as only a few phone calls were made. 
drop if fortnight==1

tab fortnight,gen(fort_)
*define the week that treatment "started" for each intervention. this was randomly varied (trans_week_start) for the transport treatment 
gen t_week =. 
replace t_week = 16+trans_start_week if tg_1==1
replace t_week = 21 if tg_2==1 
*define a running variable corresponding to the number of weeks before/since the treatment started. 
gen weeksince = week-t_week 

gen fortsince = floor((weeksince/2))
*cluster at the EA-fortnight level
gen cluster_id_f = (fortnight*1000)+cluster_id

*in a very few cases we phoned some individuals twice in one fortnight- we will exclude these from analysis. 
rename r_id id 
duplicates tag id fortnight, gen(person_fort_dup)
egen person_fort_tag = tag(id fortnight)

************************************************************************************

		* FORTNIGHTLY DATA STARTS HERE. PRESERVE TO RETURN TO MONTHLY LATER.

************************************************************************************

preserve
*interactions between treatments and fornight since treatment. 
forvalues y=1/11{
gen t_fort_m`y' = fortsince==-`y'&treat_groupind!=5

}

forvalues y=0/18{
gen t_fort_`y' = fortsince==`y'&treat_groupind!=5

}

unab treats: tg_*
unab weeks: t_fort_*

foreach x in `treats'{
foreach t in `weeks'{
gen `x'_`t' = `x'*`t'
}
}

foreach varname of varlist tg_*_* fort_*  ///
	{
		gen SQ_`varname' 	= `varname'
	}
	

	
* drop those coefficients that Stata is dropping anyway (i.e. "(omitted)" in the estimation...)
drop SQ_tg*_m*

drop SQ_tg_2_t_fort_17-SQ_tg_2_t_fort_18
 
drop SQ_fort_27

drop SQ_t*_fort_0
  
 *globals that allow for changing the y access scale and appearance 
 global bound 0.1
 global bound
 global bounding ylabel(-$bound(0.05)$bound)  yscale(range(-$bound $bound))
 global bounding "" 
 
  
 *************************************************************************************
 
 *Figures 1 and 2 in the main paper: trajectories of search effort for both treatments
 
 *************************************************************************************
 
 *Rough estimate of the average effect of the treatments in the first few weeks of treatment. 
 ivreg2 search_job_board fort_*  tg_1 tg_2   $balance if (treat_groupind!=5&fortsince>0&fortsince<9)|(treat_groupind==5& fortnight>7&fortnight<16) , cluster( cluster_id_f) 
sum search_job_board if treat_groupind==5& fortnight>7&fortnight<16
 

 local tab a
foreach outcome in Job_Search search_job_board ///
{

egen mean_`outcome' = mean(`outcome'), by(id fortnight)
replace `outcome' = mean_`outcome' if person_fort_dup==1
replace `outcome'= . if person_fort_tag==0

drop mean_`outcome'

			 global outcomehere "`outcome'"
			 			 loc outcomelabel: var l $outcomehere

			 			 loc outcomelabel: var l `outcome'
	capture drop te_* se_* treat_w cib_* cit_*

	forvalues y=1/27 ///
		{
			gen bs_`outcome'_`y' = fort_`y'*bs_`outcome'
		}

	ivreg2 `outcome' fort_*  tg_*_*  bs_`outcome'_1-bs_`outcome'_27 $balance , cluster( cluster_id_f) 
 	capture drop te_* se_* treat_w cib_* cit_*
	gen treat_w = .

	foreach g in 1 2   ///
		{
			gen te_`g' = . 
			gen se_`g' = . 
			local i = 1

			forvalues j=-7/14 ///
				{
					replace treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}

					capture replace te_`g' = _b[tg_`g'_t_fort_`m'`x']  in `i'
					capture replace se_`g' = _se[tg_`g'_t_fort_`m'`x']  in `i'

					local i = `i' +1
				}
		
			g cib_`g' = te_`g'  - 1.96*se_`g'
			g cit_`g' = te_`g'  + 1.96*se_`g'
		}

	DefineQuadraticConstraints `outcome'

	cnsreg `outcome' SQ_tg_*_* SQ_fort_*  , cluster(cluster_id_f) constraints(MyBigConstraint)
	capture drop SQ_te_* SQ_se_* SQ_treat_w SQ_cib_* SQ_cit_*
	gen SQ_treat_w = .
	foreach g in 1 2    ///
		{
			gen SQ_te_`g' = . 
			gen SQ_se_`g' = . 
			local i = 1

			forvalues j=-7/14 ///
				{
					replace SQ_treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}

					capture replace SQ_te_`g' = _b[SQ_tg_`g'_t_fort_`m'`x']  in `i'
					capture replace SQ_se_`g' = _se[SQ_tg_`g'_t_fort_`m'`x']  in `i'

					local i = `i' +1
				}
		
			gen SQ_cib_`g' = SQ_te_`g'  - 1.96 * SQ_se_`g'
			gen SQ_cit_`g' = SQ_te_`g'  + 1.96 * SQ_se_`g'
		}
	
			capture drop flat
			gen flat = 0
				
	foreach g in 1 2  ///
			{
			local second_date 0
			
			if `g'==1 ///
				{
					local second_date 8
				}

			twoway	(rarea SQ_cit_`g' SQ_cib_`g' SQ_treat_w if SQ_treat_w>-8, color(dkblue%50)   ) ///
					(line  SQ_cit_`g' SQ_treat_w if SQ_treat_w>-8, lpattern(dash) color(green)) /// 
					(line  SQ_cib_`g' SQ_treat_w if SQ_treat_w>-8, lpattern(dash) color(green)) /// 
					(line  SQ_te_`g' treat_w, color(gold)) ///
					(rcap cit_`g' cib_`g' treat_w if treat_w>-8, xmtick(-7/14) xlabel(-7/14) xtitle(Fortnight)) ///
					(scatter  te_`g' treat_w,  $bounding ///
								xline(`second_date', lcolor(magenta) lpattern(dash)) ///
								xline(0, lcolor(cyan) lpattern(dash))  ///
								plotregion(fcolor(white) lcolor(white)) ///
								graphregion(fcolor(white) lcolor(white)) ///
								legend(off)) 	(line  flat treat_w if treat_w>-8, color(gs5))  ,   yline(0, lcolor(gs5) ) 	ytitle("Effect on `outcomelabel'") 
								
			local savename1 = "results/figures/figure`g'`tab'.png"  

			graph export "`savename1'", replace width(1000)
		}	
					local tab b

}
 
 

*FIGURE A.6
local tab 6
foreach outcome in work  ///
{
egen mean_`outcome' = mean(`outcome'), by(id fortnight)
replace `outcome' = mean_`outcome' if person_fort_dup==1
replace `outcome'= . if person_fort_tag==0

drop mean_`outcome'

			 global outcomehere "`outcome'"
			 			 loc outcomelabel: var l $outcomehere

			 			 loc outcomelabel: var l `outcome'
	capture drop te_* se_* treat_w cib_* cit_*

	forvalues y=1/27 ///
		{
			gen bs_`outcome'_`y' = fort_`y'*bs_`outcome'
		}

	ivreg2 `outcome' fort_*  tg_*_*  bs_`outcome'_1-bs_`outcome'_27 $balance , cluster( cluster_id_f) 
 	capture drop te_* se_* treat_w cib_* cit_*
	gen treat_w = .

	foreach g in 1    ///
		{
			gen te_`g' = . 
			gen se_`g' = . 
			local i = 1

			forvalues j=-7/14 ///
				{
					replace treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}

					capture replace te_`g' = _b[tg_`g'_t_fort_`m'`x']  in `i'
					capture replace se_`g' = _se[tg_`g'_t_fort_`m'`x']  in `i'

					local i = `i' +1
				}
		
			g cib_`g' = te_`g'  - 1.96*se_`g'
			g cit_`g' = te_`g'  + 1.96*se_`g'
		}

	DefineQuadraticConstraints `outcome'

	cnsreg `outcome' SQ_tg_*_* SQ_fort_*  , cluster(cluster_id_f) constraints(MyBigConstraint)
	capture drop SQ_te_* SQ_se_* SQ_treat_w SQ_cib_* SQ_cit_*
	gen SQ_treat_w = .
	foreach g in 1    ///
		{
			gen SQ_te_`g' = . 
			gen SQ_se_`g' = . 
			local i = 1

			forvalues j=-7/14 ///
				{
					replace SQ_treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}

					capture replace SQ_te_`g' = _b[SQ_tg_`g'_t_fort_`m'`x']  in `i'
					capture replace SQ_se_`g' = _se[SQ_tg_`g'_t_fort_`m'`x']  in `i'

					local i = `i' +1
				}
		
			gen SQ_cib_`g' = SQ_te_`g'  - 1.96 * SQ_se_`g'
			gen SQ_cit_`g' = SQ_te_`g'  + 1.96 * SQ_se_`g'
		}
	
			capture drop flat
			gen flat = 0
				
 	foreach g in 1   ///
			{
			local second_date 8

			twoway	(rarea SQ_cit_`g' SQ_cib_`g' SQ_treat_w if SQ_treat_w>-8, color(gs15)   ) ///
					(line  SQ_cit_`g' SQ_treat_w if SQ_treat_w>-8, lpattern(dash) color(gs10)) /// 
					(line  SQ_cib_`g' SQ_treat_w if SQ_treat_w>-8, lpattern(dash) color(gs10)) /// 
					(line  SQ_te_`g' treat_w, color(gs1)) ///
					(rcap cit_`g' cib_`g' treat_w if treat_w>-8, xmtick(-7/14) xlabel(-7/14) xtitle(Fortnight)) ///
					(scatter  te_`g' treat_w,  $bounding ///
								xline(`second_date', lcolor(orange) lpattern(dash)) ///
								xline(0, lcolor(green) lpattern(dash))  ///
								plotregion(fcolor(white) lcolor(white)) ///
								graphregion(fcolor(white) lcolor(white)) ///
								legend(off)) 	(line  flat treat_w if treat_w>-8, color(gs5))  ,   yline(0, lcolor(gs5) ) 	///
								ytitle("Effect on `outcomelabel'") 
								
			local savename1 = "results/figures/figure_a`tab'_q.png"  
			graph export "`savename1'", replace width(1000)
			
		 		
			twoway	(rcap cit_`g' cib_`g' treat_w if treat_w>-8, xmtick(-7/14) xlabel(-7/14) xtitle(Fortnight)) ///
					(scatter  te_`g' treat_w,  $bounding ///
								xline(`second_date', lcolor(orange) lpattern(dash)) ///
								xline(0, lcolor(green) lpattern(dash))  ///
								plotregion(fcolor(white) lcolor(white)) ///
								graphregion(fcolor(white) lcolor(white)) ///
								legend(off)) 	(line  flat treat_w if treat_w>-8, color(gs5))  ,   yline(0, lcolor(gs5) ) 	///
								ytitle("Effect on `outcomelabel'") 
			local savename1 = "results/figures/figure_a`tab'.png"  
			graph export "`savename1'", replace width(1000)
								
		}	
		local tab 7

}


restore 

**** TABLE A.7 NEEDS TO BE DONE AT THE MONTHLY LEVEL BECAUSE THAT IS THE FREQUENCY OF THE DATA 
 
***Create Monthly data
gen monthsince = floor(((weeksince)/4))+1
tab monthsince
gen month= floor((week)/4)+1
tab month,gen(month_)

*duplicate cases:
duplicates tag id month, gen(person_month_dup)
egen person_month_tag = tag(id month)

gen cluster_id_m = (month*10000)+cluster_id

forvalues y=1/6{
gen t_month_m`y' = monthsince==-`y'&treat_groupind!=5

}

forvalues y=0/8{
gen t_month_`y' = monthsince==`y'&treat_groupind!=5

}

unab treats: tg_*
unab months: t_month_*

foreach x in `treats'{
foreach t in `months'{
gen `x'_`t' = `x'*`t'
}
}

foreach varname of varlist tg_*_* month_*  ///
	{
		gen SQ_`varname' 	= `varname'
	}

drop SQ_tg_*_t_month_m*
drop SQ_t*_month_0
drop SQ_tg_*_t_month_8 SQ_month_13



 global bound
 global bounding "" 

foreach outcome in any_travel ///
{

egen mean_`outcome' = mean(`outcome'), by(id month)
replace `outcome' = mean_`outcome' if person_month_dup==1
replace `outcome'= . if person_month_tag==0
drop mean_`outcome'

	capture drop te_* se_* treat_w cib_* cit_*

	forvalues y=1/13 ///
		{
			gen bs_`outcome'_`y' = month_`y'*bs_`outcome'
		}

	ivreg2 `outcome' month_*  tg_*_*   bs_`outcome'_1-bs_`outcome'_13  $balance , cluster( cluster_id_m) 

	capture drop te_* se_* treat_w cib_* cit_*
	gen treat_w = .

	foreach g in 1    ///
		{
			gen te_`g' = . 
			gen se_`g' = . 
			local i = 1

			forvalues j=-5/7 ///
				{
					replace treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}
					capture replace te_`g' = _b[tg_`g'_t_month_`m'`x']  in `i'
					capture replace se_`g' = _se[tg_`g'_t_month_`m'`x']  in `i'

					local i = `i' +1
				}
		
			g cib_`g' = te_`g'  - 1.96*se_`g'
			g cit_`g' = te_`g'  + 1.96*se_`g'
		}
 
	DefineQuadraticConstraintsM `outcome'

	cnsreg `outcome' SQ_tg_*_* SQ_month_*  , cluster(cluster_id_m) constraints(MyBigConstraint)

	matrix list MyBigConstraint
	
	capture drop SQ_te_* SQ_se_* SQ_treat_w SQ_cib_* SQ_cit_*
	gen SQ_treat_w = .

	foreach g in 1    ///
		{
			gen SQ_te_`g' = . 
			gen SQ_se_`g' = . 
			local i = 1

			forvalues j=-5/7 ///
				{
					replace SQ_treat_w = `j' in `i'
					local m ""
					local x = `j'
					if `x'<0 ///
						{
							local x = -`x'
							local m "m"
						}

					capture replace SQ_te_`g' = _b[SQ_tg_`g'_t_month_`m'`x']  in `i'
					capture replace SQ_se_`g' = _se[SQ_tg_`g'_t_month_`m'`x']  in `i'

					local i = `i' +1
				}
		
			gen SQ_cib_`g' = SQ_te_`g'  - 1.96 * SQ_se_`g'
			gen SQ_cit_`g' = SQ_te_`g'  + 1.96 * SQ_se_`g'
		}

 
	capture drop flat
			gen flat = 0
			
	
	foreach g in 1     ///
		{
 			local second_date 4

			twoway	(rarea SQ_cit_`g' SQ_cib_`g' SQ_treat_w if SQ_treat_w>-4, color(gs15)   ) ///
					(line  SQ_cit_`g' SQ_treat_w if SQ_treat_w>-4, lpattern(dash) color(gs10)) /// 
					(line  SQ_cib_`g' SQ_treat_w if SQ_treat_w>-4, lpattern(dash) color(gs10)) /// 
					(line  SQ_te_`g' treat_w if SQ_treat_w>-4, color(gs1)) ///
					(rcap cit_`g' cib_`g' treat_w if treat_w>-4,  xmtick(-3/7) xlabel(-3/7)  xtitle(Month)) ///
					(scatter  te_`g' treat_w if treat_w>-4, $bounding  ///
								xline(`second_date', lcolor(orange) lpattern(dash)) ///
								xline(0, lcolor(green) lpattern(dash))  ///
								plotregion(fcolor(white) lcolor(white)) ///
								graphregion(fcolor(white) lcolor(white)) ///
								legend(off)) 	(line  flat treat_w if treat_w>-8, color(gs5))  , yline(0, lcolor(gs5) ) 	 ///
								
			*version with the quadratic trend line
			local savename1 = "results/figures/figure_a`tab'_q.png"  
			graph export "`savename1'", replace width(1000)
			

			twoway	(rcap cit_`g' cib_`g' treat_w if treat_w>-4,  xmtick(-3/7) xlabel(-3/7)  xtitle(Month)) ///
					(scatter  te_`g' treat_w if treat_w>-4, $bounding  ///
								xline(`second_date', lcolor(orange) lpattern(dash)) ///
								xline(0, lcolor(green) lpattern(dash))  ///
								plotregion(fcolor(white) lcolor(white)) ///
								graphregion(fcolor(white) lcolor(white)) ///
								legend(off)) 	(line  flat treat_w if treat_w>-8, color(gs5))  , yline(0, lcolor(gs5) ) 	 ///
								
			local savename1 = "results/figures/figure_a`tab'.png"  
			graph export "`savename1'", replace width(1000)


		}	
}

	