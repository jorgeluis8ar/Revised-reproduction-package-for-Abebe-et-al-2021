
* Commented by: Jorge Luis Ochoa Rincón

********************************************************************************
* Define a program to make ITT tables ******************************************
********************************************************************************


/*

The program defines the results for Table a.27 - a.32 of the document. Speciffically, the table
reports the Intent To Treatment (ITT) estimates of the impact of the treatment random assignment
on the effects of attrition

Regressions estimated are obtained by OLS of the following equation:

Y_ic = β_0 + β_1 x treatment_{fic} + δ x X_{ic0} + μ_{ic}


Rows:

1. Imputed written agreement
2. Imputed written agreement +/- 0.25 SDs
3. Imputed written agreement +/- 0.5 SDs
4. Mean control written agreement +/- 0.25 SDs
5. Mean control written agreement +/- 0.5 SDs

Columns:

1. Control mean of each of the independent variables in local `varlist'.
2. ITT estimate of the treatment random assignment
3. Standard error of the ITT estimate of the treatment random assignment.
   
The program runs all the following steps:

1. Definition of locals.
2. Definition of matrices. This matrices will be used to create a unique matrix with all results
3. Matrices rownames and colnames are assigned.
4. In order to fill in the matrices, a loop goes through baseline covariates and fills in the corresponding value
   in each of the seven matrices. Matrices stars are used to give the legend of statistical significance in
   the document.
5. Lastly, all the matrices are merged and the finall matrix is given the right format.


*/

cap program drop itt_maker_onetreat
program define itt_maker_onetreat, rclass
		syntax varlist, TREAT(varlist)  COVARIATES(varlist) FILENAME(name) DECIMALS(integer) 
		/* 
			* varlist: local of dependent variables  --> One can acces this local by `varlist'
			* treat: the first binary treatment we are interest in --> One can acces this local by `treat'
			* covariates: local of control variables  --> One can acces this local by `covariates'
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
	
		local dep_vars 	`varlist' // define local of dependent variables
		* The local of dep_vars takes into account all variable outcomes defined in the rows section
		
		local regressors `treat'  `covariates' // define local of regressors
 		* The local of regressors takes into account all right hand side variables of equation 1
		
		local dec `decimals'
		cap drop  pval 
		cap gen pval =.

		* Initialize matrices **************************************************
		
		mat control_mean	= J(`:word count `dep_vars'',1,.) // Matrix for column 1: Control mean
		mat reg_count 		= J(`:word count `dep_vars'',1,.) // Matrix regression number of observations

		mat reg1 			= J(`:word count `dep_vars'',2,.) // Matrix for column 2 and 3: estimates of treatment branch
 
		mat stars1 			= J(`:word count `dep_vars'',2,.) // Matrix for legend of statistical significance
 
		* Initializing row names. **********************************************
		
		mat rownames control_mean = `dep_vars'
		mat colnames control_mean = "Control mean"
	
		mat rownames reg_count = `dep_vars'
		mat colnames reg_count = "N"
	
		mat rownames reg1 = `dep_vars'
		mat colnames reg1 = "Coeff of Treatment" "SE" 
	 
		/*
		
		The below loop will go trough every row of the established matrices (or simply the dependent variables).
		Every step of the loop fills in the respective value in each of the matrices
		
		*/ 
		loc i = 1
		foreach y in `dep_vars'{ 

			cap sum b_`y',d 
			capture confirm variable b_`y'
		
			/*
			The previuos line of code is an excellent way of telling stata to do two different things if the condition is
			met. Thus, if the variable is the baseline outcome variable is in the data set, then run the regression model
			taking the baseline outcome variable as a covariates. Conversly, if the condition is not met (e.g. the baseline
			variable is not in the data set), then run the regression model without the baseline variable as covariate.
			
			IT REALLY IS A COOL WAY TO HANDLE THIS PROBLEM
			*/
			
			if !_rc {
				reg `y' `regressors' [pw=ed_weight], cluster(cluster_id) 
            }
            else {
				reg `y' `regressors' [pw=ed_weight], cluster(cluster_id) 
            }

			/*
			
			Regression structure:
			
			The estimation to estimate is the following:
			
			Y_ic = β_0 + β_1 x treatment_{fic} + δ x X_{ic0} + μ_{ic}
			
			The elements of the regression are:
				1. Dependent variable: 3hanges with every step of the loop. It is represented by the local `y'
				2. Regressors: Defined as the covariates input in the program plus the a dummy variable if 
				   an individual was ever assigned to treatment.
				4. Regression weights: Given the sampling frequency of differente groups of the population
				   to be represented in the study sample, inverse weights help to estimate the ITT according to
				   the proportion of this subgroups in the sample. This is done by the option in brackets pw
				5. Standard cluster errors: As the randomization of the sample and treatments was done in two steps.
				   Firstly geographical zones and secondly within zones teatment randomization, the standar
				   errors should be clustered to allow correlation within geographical zones of individuals.
				   This is done by the option cluster(cluser_var)
			*/
			
			* inputting values into the matrix piece by piece ******************

			* control mean -----------------------------------------------------
			
			qui sum `y' if (`treat ' == 0  ) 
			mat control_mean[`i', 1] 	= r(mean)   // Saving the value to the mean matrix

			* count of regression ----------------------------------------------
			
			mat reg_count[`i', 1] 		= e(N)      // Saving the number of observations in the regression

			* P values and stars  ----------------------------------------------
			
			local p1 = (2 * ttail(e(df_r), abs(_b[`treat']/_se[`treat'])))
			/*
			Local p1 is not more than taking the inverse normal function in order to find the
			p value assotiated with a t statistic given in each regression for coefficient
			of the treatment dummy variable.
			*/
			
			mat stars1[`i', 2] = 0 					  // make the first column of stars 0
			if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
		    else                mat stars1[`i',1] = 0 // if not, no stars
			if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
			if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
			
			
	 		replace pval =  `p1' if _n==`i' 

			* Regression parameter estimates -----------------------------------
			
			mat reg1[`i',1] = _b[`treat']
			mat reg1[`i',2] = _se[`treat']
 	
			local i = `i' + 1 				// Increasing the value of local `i' for the next loop iteration
		}

		* Merge matrices together to form larger matrix ------------------------
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
		
	 
		* Merge matrices together to form larger matrix without N  -------------
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
