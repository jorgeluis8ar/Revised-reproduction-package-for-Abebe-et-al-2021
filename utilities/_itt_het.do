
	

********************************************************************************
* Define a program to make ITT tables ******************************************
********************************************************************************

	cap program drop itt_maker_het
	program define itt_maker_het, rclass
		syntax varlist, TREAT1(varlist) TREAT2(varlist)  OUTCOME(varlist) COVARIATES(varlist) FILENAME(name) DECIMALS(integer)  [NON] [CP]
		/* 
			* varlist: local of dependent variables 
			* treat1: the first binary treatment we are interest in
			* treat2: the second binary treatment we are interest in
			* covariates: local of control variables 
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
	foreach h in 0 1{

		local het_vars 	`varlist' // define local of dependent variables
	
		loc regressors `treat1'  `treat2'    `covariates' // define local of regressors
	

		
		local dec `decimals'
		cap drop  pval1
		cap gen pval1 =.

		cap drop  pval2
		cap gen pval2 =.
		
		* Initialize matrices **************************************************
		mat control_mean	= J(`:word count `het_vars'',3,.) // generate matrix
		mat reg_count 		= J(`:word count `het_vars'',3,.)

		mat reg1 			= J(`:word count `het_vars'',3,.)
 		mat reg2 			= J(`:word count `het_vars'',3,.)

		mat stars1 			= J(`:word count `het_vars'',3,.)
 		mat stars2 			= J(`:word count `het_vars'',3,.)

		*mat pval 			= J(`:word count `het_vars'',3,.)

		mat cpval 			= J(`:word count `het_vars'',6,.)
 
		* Initializing row names. **********************************************
		mat rownames control_mean = `het_vars'
		mat colnames control_mean = "Control mean"
	
		mat rownames reg_count = `het_vars'
		mat colnames reg_count = "N"
	
		mat rownames reg1 = `het_vars'
		mat colnames reg1 = "Transport" "SE" "qval"
	 
		mat rownames reg2 = `het_vars'
		mat colnames reg2 = "Workshop" "SE" "qval"
 

		
		/* 	Thist next bit constructs a list of the variable labels of the 
			dependent variables labels. The way stata deals with lists of 
			string is tricky, but this code works. I think I am tricking stata 
			into enclosing an empty string with quotation marks. */
		
		/* Defining the row names and the column names of the matrix */

		loc i = 1
		foreach y in `het_vars'{ 
		cap drop `treat1'_h_*
		cap drop `treat2'_h_*
		gen `treat1'_h_0 = `treat1'==1&`y'==0
		gen `treat1'_h_1 = `treat1'==1&`y'==1
		gen `treat2'_h_0 = `treat2'==1&`y'==0
		gen `treat2'_h_1 = `treat2'==1&`y'==1
		
 
			* inputting values into the matrix piece by piece ******************
			qui sum `outcome' if (treat_groupind == 5  )& `y'==`h'   [aw=ed_weight]  // define the control group
			* control mean
			mat control_mean[`i', 1] 	= r(mean)
		display "`outcome'"

		cap sum bs_`outcome',d 
		capture confirm variable bs_`outcome'
		
	
		if !_rc {
               	reg `outcome' bs_`outcome' `treat1'_h_0 `treat1'_h_1   `treat2'_h_0  `treat2'_h_1 `y'   `covariates'  [pw=ed_weight]  ,   cluster(cluster_id) 

               }
               else {
			   			reg `outcome'   `treat1'_h_0 `treat1'_h_1   `treat2'_h_0  `treat2'_h_1   `y' `covariates' [pw=ed_weight]  ,   cluster(cluster_id) 
                }

			local p1 = (2 * ttail(e(df_r), abs(_b[`treat1'_h_`h']/_se[`treat1'_h_`h']))) 	// calculate p value for treatment 1
			local p2 = (2 * ttail(e(df_r), abs(_b[`treat2'_h_`h']/_se[`treat2'_h_`h'])))	// calculate p value for treatment 2

			* count of regression
			mat reg_count[`i', 1] 		= e(N)
			* P values and stars 
				local p1 = (2 * ttail(e(df_r), abs(_b[`treat1'_h_`h']/_se[`treat1'_h_`h'])))
				replace pval1 =  `p1' if _n==`i' 

				mat stars1[`i', 2] = 0 // make the first column of stars 0
				mat stars1[`i', 3] = 0 // make the first column of stars 0

				if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
					else mat stars1[`i',1] = 0 // if not, no stars
				if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
				if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
			
				local p2= (2 * ttail(e(df_r), abs(_b[`treat2'_h_`h']/_se[`treat2'_h_`h'])))
				replace pval2 =  `p2' if _n==`i' 

				mat stars2[`i', 2] = 0 // make the first column of stars 0
				mat stars2[`i', 3] = 0 // make the first column of stars 0

				if (`p2' < .1) 		mat stars2[`i',1] = 1 // less than 10%?
					else mat stars2[`i',1] = 0 // if not, no stars
				if (`p2' < .05) 	mat stars2[`i',1] = 2 // less than 5%?
				if (`p2' < .01) 	mat stars2[`i',1] = 3 // less than 1%?

				* Regression parameter estimates 
				mat reg1[`i',1] = _b[`treat1'_h_`h']
				mat reg1[`i',2] = _se[`treat1'_h_`h']

				mat reg2[`i',1] = _b[`treat2'_h_`h']
				mat reg2[`i',2] = _se[`treat2'_h_`h']
				
			test  `treat1'_h_0 = `treat1'_h_1
			mat cpval[`i',1] = r(p)	
			
			test  `treat2'_h_0 = `treat2'_h_1
			mat cpval[`i',4] = r(p)	
	
			local i = `i' + 1 
		}
	 
		
	*** creat q-values 	
	foreach p in 1 2{
	preserve 
	keep pval`p'  `het_vars'  `regressors'   
 	
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
	replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
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
	replace bh95_qval = `qval' if rank <= total_rejected & rank~=.
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

 		* Merge matrices together to form larger matrix
		frmttable, statmat(control_mean)  sdec( `dec' , `dec', `dec'  ) varlabels substat(2) `merge'
		if "`non'"!="non"{
		frmttable, statmat(reg_count) sdec(0) varlabels substat(2)  merge 
		}
		frmttable, statmat(reg1) sdec(`dec'\   `dec' \  3) annotate(stars1) asymbol(*,**,***) varlabels merge substat(2)
		frmttable, statmat(reg2)  sdec(`dec'\   `dec' \  3) annotate(stars2) asymbol(*,**,***) varlabels merge substat(2)
		
		local merge merge
}
		if "`cp'"=="cp"{
		 frmttable, statmat(cpval)   varlabels merge substat(2)
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
		"Baseline covariate", "mean", "N", "", "","mean", "N", \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
		multicol(1,2,4;1,6,4) 
		}
		else{
		if "`cp'"!="cp"{
		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "Covariate = 0", "", "",  "Covariate = 1", "", "", ""\ ///
		"\cmidrule(lr){2-4}\cmidrule(lr){5-7}", "Control", "{Transport}", "{Workshop}", "Control",  "{Transport}", "{Workshop}"\ ///
		"Baseline covariate", "mean", "", "", "mean" \ ///
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
		ctitle("", "Covariate = 0", "","" ,  "Covariate = 1", "",  "","Transport" , "Workshop"\ ///
		"\cmidrule(lr){2-4}\cmidrule(lr){5-7}\cmidrule(lr){8-9}", "Control", "{Transport}", "{Workshop}", , "Control",  "{Transport}", "{Workshop}" ,"{Equality}","{Equality}"\ ///
		"Baseline covariate", "mean", "", "",   "mean", "", "", "(pval)", "(pval)" \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)") ///
		multicol(1,2,3;1,5,3) 
		
		}
		}
		
		display "end"
		
	 

	end
********************************************************************************
