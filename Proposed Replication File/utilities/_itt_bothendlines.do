
* Commented by: Jorge Luis Ochoa Rincón

********************************************************************************
* Define a program to make tables ******************************************
********************************************************************************

cap program drop itt_maker_jobstime
program define itt_maker_jobstime, rclass
		syntax varlist, TREAT1(varlist) TREAT2(varlist) COVARIATES(varlist) FILENAME(name) DECIMALS(integer)  [NON] [CP]
		/* 
			* varlist: local of dependent variables  --> One can acces this local by `varlist'
			* treat1: the first binary treatment we are interest in --> One can acces this local by `treat1'
			* treat2: the second binary treatment we are interest in --> One can acces this local by `treat2'
			* covariates: local of control variables  --> One can acces this local by `covariates'
		*/	
foreach t in 2 6{
	
		local dep_vars 	`varlist' // define local of dependent variables
		* The local of dep_vars takes into account all variable outcomes defined in the rows section
		
		local regressors `treat1' `treat2'   `covariates' // define local of regressors
		* The local of regressors takes into account all variables in the rigth hand side of equation 1
		
		local dec `decimals'
		
		cap drop  pval1
		cap gen pval1 =.

		cap drop  pval2
		cap gen pval2 =.
		
		* Initialize matrices **************************************************
		
		mat control_mean	= J(`:word count `dep_vars'',3,.) // Matrix for columns 1 and 5: Control mean
		mat reg_count 		= J(`:word count `dep_vars'',3,.) // Matrix for the number of observations in each regression

		mat reg1 			= J(`:word count `dep_vars'',3,.) // Matrix for columns 2 and 6:Estimates of transport subsidy
 		mat reg2 			= J(`:word count `dep_vars'',3,.) // Matrix for columns 3 and 7:Estimates of workshop intervention

		mat stars1 			= J(`:word count `dep_vars'',3,.) // Matrix for the legend of statistical significance reg1
 		mat stars2 			= J(`:word count `dep_vars'',3,.) // Matrix for the legend of statistical significance reg2

		*mat pval 			= J(`:word count `dep_vars'',3,.)

		mat cpval 			= J(`:word count `dep_vars'',3,.) // Matrix for column 5: P value of linear hypotheses
 
		* Initializing row names ***********************************************
		
		mat rownames control_mean = `dep_vars'
		mat colnames control_mean = "Control mean"
	
		mat rownames reg_count = `dep_vars'
		mat colnames reg_count = "N"
	
		mat rownames reg1 = `dep_vars'
		mat colnames reg1 = "Transport" "SE" "qval"
	 
		mat rownames reg2 = `dep_vars'
		mat colnames reg2 = "Workshop" "SE" "qval"

		loc i = 1
		foreach y in `dep_vars'{ 
	
			* inputting values into the matrix piece by piece ******************
			
			quietly sum `y' if (treat_groupind == 5  ) & (time==`t')  [aw=ed_weight]
			/*
			Defining the control mean for each variable using inverse frequency weights in the sample
			*/
		
			* control mean -----------------------------------------------------
			
			mat control_mean[`i', 1] 	= r(mean)


			cap sum bs_`y',d 
			capture confirm variable bs_`y'
			
			/*
			The previuos line of code is an excellent way of telling stata to do two different things if the condition is
			met. Thus, if the variable is the baseline outcome variable is in the data set, then run the regression model
			taking the baseline outcome variable as a covariates. Conversly, if the condition is not met (e.g. the baseline
			variable is not in the data set), then run the regression model without the baseline variable as covariate.
			
			IT REALLY IS A COOL WAY TO HANDLE THIS PROBLEM
			*/
			
			if !_rc {
				reg `y' bs_`y' `regressors' [pw=ed_weight] if time==`t', cluster(cluster_id) 
			}
			else {
				reg `y' `regressors' [pw=ed_weight] if time==`t', cluster(cluster_id) 
			}
			/*
			
			Regression structure:
			
			The estimation to estimate is the following:
			
			Y_ic = β_0 + β_1 x treat\_transport_{fic} + + β_2 x treat\_workshop_{fic} + α x y_{ic,pre} + δ x X_{ic0} + μ_{ic}
			
			
			The elements of the regression are:
				1. Dependent variable: Changes with every step of the loop. It is represented by the local `y'
				2. Pre-treatment outcome variable: As explained above, capture confirm variable bs_`y' help to
				   determine if the baseline outcome variable should be included in the model.
				3. Regressors: Defined as the covariates input in the program plus the two branches of treatment;
				   1) transport subsidy or 2) workshop intervention.
				4. Regression weights: Given the sampling frequency of differente groups of the population
				   to be represented in the study sample, inverse weights help to estimate the ITT according to
				   the proportion of this subgroups in the sample. This is done by the option in brackets pw
				5. Standar cluster errors: As the randomization of the sample and treatments was done in two steps.
				   Firstly geographical zones and secondly within zones teatment randomization, the standar errors should 
				   be cluster to allow correlation within geographical zones of individuals. This is done by the option
				   cluster(cluser_var)
				6. Subsample: The regressions are measuring impacts of both interventions in different periods of time. 
				   For the first endline (2 years after the intervention) and the second endline (6 years after the intervention).
				   This is done by using the local `t' and subsampling the data with the conditional if.
			
			I eliminated two lines that were repeated below
			*/
				
			* count of regression ----------------------------------------------
			
			mat reg_count[`i', 1] 		= e(N)
			* P values and stars 
				local p1 = (2 * ttail(e(df_r), abs(_b[`treat1']/_se[`treat1'])))
				replace pval1 =  `p1' if _n==`i' 

				mat stars1[`i', 2] = 0 // make the first column of stars 0
				mat stars1[`i', 3] = 0 // make the first column of stars 0

				if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
					else mat stars1[`i',1] = 0 // if not, no stars
				if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
				if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
				

				 
				local p2= (2 * ttail(e(df_r), abs(_b[`treat2']/_se[`treat2'])))
				replace pval2 =  `p2' if _n==`i' 

				mat stars2[`i', 2] = 0 // make the first column of stars 0
				mat stars2[`i', 3] = 0 // make the first column of stars 0

				if (`p2' < .1) 		mat stars2[`i',1] = 1 // less than 10%?
					else mat stars2[`i',1] = 0 // if not, no stars
				if (`p2' < .05) 	mat stars2[`i',1] = 2 // less than 5%?
				if (`p2' < .01) 	mat stars2[`i',1] = 3 // less than 1%?

				* Regression parameter estimates 
				mat reg1[`i',1] = _b[`treat1']
				mat reg1[`i',2] = _se[`treat1']

				mat reg2[`i',1] = _b[`treat2']
				mat reg2[`i',2] = _se[`treat2']
				
			test  `treat1' = `treat2'   
			mat cpval[`i',1] = r(p)	
 
	
			local i = `i' + 1 
		}
	
	
	*** creat q-values 	
	foreach p in 1 2{
	preserve 
	keep pval`p'  `dep_vars'  `regressors'   
 	
	keep if pval`p'!=.
  
	
	quietly sum pval`p'
	local totalpvals = r(N)

	quietly gen int original_sorting_order = _n
	quietly sort pval`p'
	quietly gen int rank = _n if pval`p'~=.
	
	gen bky06_qval = 1 if pval`p'~=.

	local qval = 1

	
	while `qval' > 0 {
	* First Stage
	* Generate the adjusted first stage q level we are testing: q' = q/1+q
	local qval_adj = `qval'/(1+`qval')
	* Generate value q'*r/M
	gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= q'*r/M
	gen reject_temp1 = (fdr_temp1>=pval`p') if pval`p'~=.
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
	gen reject_temp2 = (fdr_temp2>=pval`p') if pval`p'~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	gen reject_rank2 = reject_temp2*rank
	* Record the rank of the largest p-value that meets above condition
	egen total_rejected2 = max(reject_rank2)

	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	quietly replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
	* Reduce q by 0.001 and repeat loop
	drop fdr_temp* reject_temp* reject_rank* total_rejected*
	local qval = `qval' - .001
	}
	
		local qval = 1
	gen bh95_qval = 1 if pval~=.

	while `qval' > 0 {
	* Generate value qr/M
	quietly gen fdr_temp = `qval'*rank/`totalpvals'
	* Generate binary variable checking condition p(r) <= qr/M
	quietly gen reject_temp = (fdr_temp>=pval) if fdr_temp~=.
	* Generate variable containing p-value ranks for all p-values that meet above condition
	quietly gen reject_rank = reject_temp*rank
	* Record the rank of the largest p-value that meets above condition
	quietly egen total_rejected = max(reject_rank)
	* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
	quietly replace bh95_qval = `qval' if rank <= total_rejected & rank~=.
	* Reduce q by 0.001 and repeat loop
	quietly drop fdr_temp reject_temp reject_rank total_rejected
	local qval = `qval' - .001
}


	quietly sort original_sorting_order
	
	local m = 1
	display "`totalpvals'"
	while `m' <= `totalpvals'{
	local pos = `m'  
	mat reg`p'[`pos',3] = bky06_qval[`m']
*	mat qvals[`pos',1] = bh95_qval[`m']

	local m = `m'+1 
	}
	restore
	}

	local dec 3
		* Merge matrices together to form larger matrix
		frmttable, statmat(control_mean)  sdec(3\3\3 \ 3\3\3 \ 3\3\3  ) varlabels substat(2) `merge'
		if "`non'"!="non"{
		frmttable, statmat(reg_count) sdec(0) varlabels substat(2)  merge 
		}
		frmttable, statmat(reg1) sdec(3\3\3 \ 3\3\3 \ 3\3\3  ) annotate(stars1) asymbol(*,**,***) varlabels merge substat(2)
		frmttable, statmat(reg2)  sdec(3\3\3 \ 3\3\3 \ 3\3\3  ) annotate(stars2) asymbol(*,**,***) varlabels merge substat(2)
		if "`cp'"=="cp"{
		 frmttable, statmat(cpval)   varlabels merge substat(2)
		}
		local merge merge
}
		
		if "`non'"!="non"{
		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "2015", "", "", "", "2018", "", "", ""\ ///
		"\cmidrule(lr){2-5}\cmidrule(lr){6-9}", "Control", "", "{Transport}", "{Workshop}", "Control", "", "{Transport}", "{Workshop}"\ ///
		"Outcome", "mean", "N", "", "","mean", "N", \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
		multicol(1,2,4;1,6,4) 
		}
		else{
		if "`cp'"!="cp"{
		frmttable using "tables/`filename'_non", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "2015", "", "",  "2018", "", "", ""\ ///
		"\cmidrule(lr){2-4}\cmidrule(lr){5-7}", "Control", "{Transport}", "{Workshop}", "Control",  "{Transport}", "{Workshop}"\ ///
		"Outcome", "mean", "", "", "mean" \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)") ///
		multicol(1,2,3;1,5,3) 
		}
		else{
		*append one more row
 		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "2015", "","", "",  "2018", "", "", "",""\ ///
		"\cmidrule(lr){2-5}\cmidrule(lr){6-9}", "Control", "{Transport}", "{Workshop}","{Equality}", "Control",  "{Transport}", "{Workshop}" ,"{Equality}"\ ///
		"Outcome", "mean", "", "", "(pval)", "mean", "", "", "(pval)" \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
		multicol(1,2,4;1,6,4) 
		
		}
		}
		
		display "end"
		
	 

	end
********************************************************************************
