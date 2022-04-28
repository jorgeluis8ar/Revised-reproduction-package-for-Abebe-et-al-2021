
	

********************************************************************************
* Define a program to make ITT tables ******************************************
********************************************************************************

	cap program drop itt_maker_onetreat
	program define itt_maker_onetreat, rclass
		syntax varlist, TREAT(varlist)  COVARIATES(varlist) FILENAME(name) DECIMALS(integer) 
		/* 
			* varlist: local of dependent variables 
			* treat1: the first binary treatment we are interest in
			* treat2: the second binary treatment we are interest in
			* covariates: local of control variables 
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
	
		local dep_vars 	`varlist' // define local of dependent variables
	
		loc regressors `treat'  `covariates' // define local of regressors
 		*/	
		
		local dec `decimals'
		cap drop  pval 
		cap gen pval =.


		* Initialize matrices **************************************************
		mat control_mean	= J(`:word count `dep_vars'',1,.) // generate matrix
		mat reg_count 		= J(`:word count `dep_vars'',1,.)

		mat reg1 			= J(`:word count `dep_vars'',2,.)
 
		mat stars1 			= J(`:word count `dep_vars'',2,.)
 
		mat qvals 			= J(`:word count `dep_vars'',1,.)



		* Initializing row names. **********************************************
		mat rownames control_mean = `dep_vars'
		mat colnames control_mean = "Control mean"
	
		mat rownames reg_count = `dep_vars'
		mat colnames reg_count = "N"
	
		mat rownames reg1 = `dep_vars'
		mat colnames reg1 = "Coeff of Treatment" "SE" 
	 
 
		/* 	Thist next bit constructs a list of the variable labels of the 
			dependent variables labels. The way stata deals with lists of 
			string is tricky, but this code works. I think I am tricking stata 
			into enclosing an empty string with quotation marks. */
		
		/* Defining the row names and the column names of the matrix */

		loc i = 1
		foreach y in `dep_vars'{ 
		//	svy: reg `y' `regressors'  // run the regression
		                   			*reg `y' `regressors' b_`y' , robust  

		cap sum b_`y',d 
		capture confirm variable b_`y'
		if !_rc {
               	reg `y'  `regressors'   [pw=ed_weight],   cluster(cluster_id) 
				
               }
               else {
               	reg `y'   `regressors'   [pw=ed_weight],   cluster(cluster_id) 

                }
				
			local p1 = (2 * ttail(e(df_r), abs(_b[`treat']/_se[`treat']))) 	// calculate p value for treatment 1
			qui sum `y' if (`treat ' == 0  ) // define the control group
	
			* inputting values into the matrix piece by piece ******************

			* control mean
			mat control_mean[`i', 1] 	= r(mean)

			* count of regression
			mat reg_count[`i', 1] 		= e(N)

			* P values and stars 
				local p1 = (2 * ttail(e(df_r), abs(_b[`treat']/_se[`treat'])))
				
				mat stars1[`i', 2] = 0 // make the first column of stars 0
				if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
					else mat stars1[`i',1] = 0 // if not, no stars
				if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
				if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
				
				
	 			replace pval =  `p1' if _n==`i' 

				* Regression parameter estimates 
				mat reg1[`i',1] = _b[`treat']
				mat reg1[`i',2] = _se[`treat']
 
	
			local i = `i' + 1 
		}

	
		* Merge matrices together to form larger matrix
		frmttable, statmat(control_mean) sdec(`dec') varlabels
		frmttable, statmat(reg_count) sdec(0) varlabels merge 
		frmttable, statmat(reg1) sdec(`dec') annotate(stars1) asymbol(*,**,***) varlabels merge //substat(1)
 	
		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("", "",   "{ ITT Estimate}", "", "" \ ///
		"", "Control",  " ", "",   "",   "Sharpened" \ ///
		"Outcome", "mean", "N", "Coeff", "Std. Err." \ ///
		"", "(1)", "(2)", "(3)", "(4)" ) ///
		multicol(1,3,4; 2,3,4) 
		
	 
		* Merge matrices together to form larger matrix without N  
		frmttable, statmat(control_mean) sdec(`dec') varlabels
 		frmttable, statmat(reg1) sdec(`dec')  annotate(stars1) asymbol(*,**,***)  varlabels merge //substat(1) 

		frmttable using "tables/`filename'", 	///
		tex ///
		fragment ///
		varlabels ///
		nocenter ///
		replace ///
		ctitle("",  "",   "{ ITT Estimate}", "", "" \ ///
		"\cline{3-4}", "Control",   "",   "" \ ///
		"Outcome", "mean", "Coeff", "Std. Err."\ ///
		"", "(1)", "(2)", "(3)") ///
		multicol(1,3,2) 
 

	end
********************************************************************************
