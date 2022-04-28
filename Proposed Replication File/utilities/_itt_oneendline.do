
	

********************************************************************************
* Define a program to make ITT tables ******************************************
********************************************************************************

/*

The program defines tge results for Table 3 of the document. Speciffically, the table
reports the intent/to/treatment estimates of the impact of the transport intervention
and the job application workshop on several outcomes realated to match quality.

Regressions estimated are obtained by OLS of the following equation>

Y_ic = β_0 + \sum_f[β_f x treat_{fic} + γ x spillover_{fic}] + α x y_{ic,pre} + δ x X_{ic0} + μ_{ic}

Dependent variables are measured 6 years from the initial intervention . All standard errors of
the coeficients are clustered at the geographical cluster (This geographical zones are defined as
the statification strategy for randomizing the intervention). Following is the structure of Table 3:

Rows:

1. Wages conditional on having a job.
2. Longest tenure respondant has had in the previous 2 years (conditional on having had a job in the last 2 year).
3. Longest tenure in any typw of job unconditionally on having had a job.
4. Dummy variable if individual uses skills acquired in previous jobs or school.
5. Dummy variable if individual has been promoted its current job.

*/

cap program drop itt_maker_jobs
	program define itt_maker_jobs, rclass
		syntax varlist, TREAT1(varlist) TREAT2(varlist)   COVARIATES(varlist) FILENAME(name) DECIMALS(integer) 
		/* 
			* varlist: local of dependent variables 
			* treat1: the first binary treatment we are interest in
			* treat2: the second binary treatment we are interest in
			* covariates: local of control variables 
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
	
		local dep_vars 	`varlist' // define local of dependent variables
	
		loc regressors `treat1' `treat2'   `covariates' // define local of regressors
		
		local dec `decimals'
	

		* Initialize matrices **************************************************
		mat control_mean	= J(`:word count `dep_vars'',2,.) // generate matrix
		mat reg_count 		= J(`:word count `dep_vars'',2,.)

		mat reg1 			= J(`:word count `dep_vars'',2,.)
 		mat reg2 			= J(`:word count `dep_vars'',2,.)

		mat stars1 			= J(`:word count `dep_vars'',2,.)
 		mat stars2 			= J(`:word count `dep_vars'',2,.)

		mat cpval 			= J(`:word count `dep_vars'',2,.)
 
		* Initializing row names. **********************************************
		 mat rownames control_mean = `dep_vars'
		mat colnames control_mean = "Control mean"
	
		*mat rownames reg_count = `dep_vars'
		mat colnames reg_count = "N"
	
		*mat rownames reg1 = `dep_vars'
		mat colnames reg1 = "Transport" "SE" 
	 
	 
		*mat rownames reg2 = `dep_vars'
		mat colnames reg2 = "Workshop" "SE" 
 
		*mat rownames cpval =  `dep_vars' 
		mat colnames cpval = "Joint pval"  
		
		
		/* 	Thist next bit constructs a list of the variable labels of the 
			dependent variables labels. The way stata deals with lists of 
			string is tricky, but this code works. I think I am tricking stata 
			into enclosing an empty string with quotation marks. */
		
		/* Defining the row names and the column names of the matrix */

		loc i = 1
		foreach y in `dep_vars'{ 
	 
		cap sum bs_`y',d 
		capture confirm variable bs_`y'
		
		if !_rc {
               	reg `y' bs_`y' `regressors'   [pw=ed_weight],   cluster(cluster_id) 

               }
               else {
			   			reg `y' `regressors' [pw=ed_weight],   cluster(cluster_id) 

                }
			local p1 = (2 * ttail(e(df_r), abs(_b[`treat1']/_se[`treat1']))) 	// calculate p value for treatment 1
			local p2 = (2 * ttail(e(df_r), abs(_b[`treat2']/_se[`treat2'])))	// calculate p value for treatment 2

			qui sum `y' if (treat_groupind == 5 ) [aw=ed_weight]  // define the control group
	
			* inputting values into the matrix piece by piece ******************

			* control mean
			mat control_mean[`i', 1] 	= r(mean)

			* count of regression
			mat reg_count[`i', 1] 		= e(N)

			* P values and stars 
				local p1 = (2 * ttail(e(df_r), abs(_b[`treat1']/_se[`treat1'])))
				
				mat stars1[`i', 2] = 0 // make the first column of stars 0
				if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
					else mat stars1[`i',1] = 0 // if not, no stars
				if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
				if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
				
				 
				local p2= (2 * ttail(e(df_r), abs(_b[`treat2']/_se[`treat2'])))
	
				mat stars2[`i', 2] = 0 // make the first column of stars 0
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
				*test 
				*mat reg2[`i',1] = _b[`treat2']
				*mat reg2[`i',2] = _se[`treat2']
 
	
			local i = `i' + 1 
		}
	
	
		* Merge matrices together to form larger matrix
		frmttable, statmat(control_mean) sdec(`dec') varlabels substat(1)
				frmttable, statmat(reg_count) sdec(0) varlabels merge   substat(1)
		frmttable, statmat(reg1) sdec(`dec') annotate(stars1) asymbol(*,**,***) varlabels merge substat(1)
		frmttable, statmat(reg2) sdec(`dec') annotate(stars2) asymbol(*,**,***) varlabels merge substat(1)
 		frmttable, statmat(cpval) sdec(3)   varlabels merge  substat(1)
 

		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "", "", "{ITT Estimates}", "", ""  \ ///
		"", "Control", "", "{Transport}" , "{Workshop}" \ ///
		"Outcome", "mean", "N", "Coeff",  "Coeff",  "Equality pval" \ ///
		"", "(1)", "(2)", "(3)", "(4)", "(5)" ) ///
		multicol(1,4,2) 
		
		
		
		display "end"
		
	 

	end
********************************************************************************
