
	

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

Columns:

1. Control mean of each of the independent variables in local `varlist'.
2. Number of observations used in the regression model.
3. Coefficient of the transport intervention
4. Coefficient of the workshop intervention
5. P value of a difference in means t test.

*/

cap program drop itt_maker_jobs
program define itt_maker_jobs, rclass
	syntax varlist, TREAT1(varlist) TREAT2(varlist) COVARIATES(varlist) FILENAME(name) DECIMALS(integer) 
		/* 
			* varlist: local of dependent variables --> One can acces this local by `varlist'
			* treat1: the first binary treatment we are interest in  --> One can acces this local by `treat1'
			* treat2: the second binary treatment we are interest in --> One can acces this local by `treat2'
			* covariates: local of control variables  --> One can acces this local by `covariates'
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
	
		local dep_vars 	`varlist' // define local of dependent variables
		* The local of dep_vars takes into account all variable outcomes defined in the rows section
		
		loc regressors `treat1' `treat2' `covariates' // define local of regressors
		* The local of regressor takes into account all rigth term of equation one.
		
		local dec `decimals'
	

		* Initialize matrices **************************************************

		mat control_mean	= J(`:word count `dep_vars'',2,.) // Matrix for column 1: Control mean
		mat reg_count 		= J(`:word count `dep_vars'',2,.) // Matrix for column 2: Number of observations in regressions

		mat reg1 			= J(`:word count `dep_vars'',2,.) // Matrix for column 3: Estimates of transport intervention
 		mat reg2 			= J(`:word count `dep_vars'',2,.) // Matrix for column 4: Estimates of workshop intervention

		mat stars1 			= J(`:word count `dep_vars'',2,.)
 		mat stars2 			= J(`:word count `dep_vars'',2,.)

		mat cpval 			= J(`:word count `dep_vars'',2,.) // Matrix for column 5: P value of ttest
 
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
		
		/*
		
		The below loop will go trough every row of the established matrices (or simply the dependent variables).
		Every step of the loop fills in the respective value in each of the matrices
		
		*/
		foreach y in `dep_vars'{ 
	 
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
				reg `y' bs_`y' `regressors' [pw=ed_weight], cluster(cluster_id) 
			}
			else {
				reg `y' `regressors' [pw=ed_weight], cluster(cluster_id) 
			}
			
			
			/*
			
			Regression structure:
			
			The estimation to estimate is the following:
			
			Y_ic = β_0 + β_1 x treat\_transport_{fic} + + β_2 x treat\_workshop_{fic}  + γ x spillover_{fic}] + α x y_{ic,pre} + δ x X_{ic0} + μ_{ic}
			
			
			The elements of the regression are:
				1. Dependent variable: Changes with every step of the loop. It is represented by the local `y'
				2. Pre treatment outcome variable: As explained above, capture confirm variable bs_`y' help to
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
			
			*/
			
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
 
	
			local i = `i' + 1   // increasing the local `i' value for the next row in all matrices
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
