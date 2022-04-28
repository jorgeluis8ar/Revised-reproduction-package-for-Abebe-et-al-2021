*
* Comented by: Jorge Luis Ochoa Rincon
*

********************************************************************************
* Define a program to make ITT tables ******************************************
********************************************************************************

/*

The program defines the results for Table 4 of the document. Speciffically, the table
reports the Intent To Treatment (ITT) estimates of the impact of the transport intervention
and the job application workshop on several outcomes by baseline characteristics.

Regressions estimated are obtained by OLS of the following equation:

Y_ic = β_0 + \sum_f[β_f x treat_{fic} + γ x spillover_{fic}] + α x y_{ic,pre} + δ x X_{ic0} + μ_{ic}

The dependent variable are wage earnings, permanent work status and written agreement contract
measured 6 years from the initial intervention. All standard errors of the coeficients are
clustered at the geographical cluster (This geographical zones are defined as the statification
strategy for randomizing the intervention). Following is the structure of Tables 4,5,6:

The idea of the table is to present how estimates change by thre presence of baseline covariates in 
the individuals. In detail, the table compares individuals who are male to those who are not. The table
compares individuals who are active job searchers to those who are not.


Rows:

1. Baseline characteristic of whether the person studied at the tertiary level
2. Baseline characteristic of whether the person is female of male
3. Baseline characteristic of whether the person is an active job searcher
4. Baseline characteristic of whether the person has ever had a permanent job
5. Baseline characteristic of whether the person lives close to the CDB

Columns:

1. Control mean of each of the independent variables in local `varlist' for the subsample of baseline covarite =0.
2. Coefficient of the transport intervention for the subsample of baseline covarite =0
3. Coefficient of the workshop intervention for the subsample of baseline covarite =0
4. Control mean of each of the independent variables in local `varlist' for the subsample of baseline covarite =1.
5. Coefficient of the transport intervention for the subsample of baseline covarite =1
6. Coefficient of the workshop intervention for the subsample of baseline covarite =1
7. P value of a linear hypotheses for equal estimates (are effects from the transport
   subsides in the sample of baseline covariate =0 equal to the effects from the transport
   subsides in the sample of baseline covariate =1?) .
8. P value of a linear hypotheses for equal estimates (are effects from the workshop
   intervention in the sample of baseline covariate =0 equal to the effects from the workshop
   intervention in the sample of baseline covariate =1?) .
   
The program runs all the following steps:

1. Definition of locals.
2. Definition of matrices. This matrices will be used to create a unique matrix with all results
3. Matrices rownames and colnames are assigned.
4. In order to fill in the matrices, a loop goes through baseline covariates equal to 1 or 0
   and fills in the corresponding value in each of the seven matrices. Matrices stars are
   used to give the legend of statistical significance in the document.
5. Q values are created in a two stage methodology
6. Lastly, all the matrices are merged and the finall matrix is given the right format.


*/

cap program drop itt_maker_het
program define itt_maker_het, rclass
		syntax varlist, TREAT1(varlist) TREAT2(varlist)  OUTCOME(varlist) COVARIATES(varlist) FILENAME(name) DECIMALS(integer) [NON] [CP]
		
		/* 
			* varlist: local of dependent variables --> One can acces this local by `varlist'
			* treat1: the first binary treatment we are interest in --> One can acces this local by `treat1'
			* treat2: the second binary treatment we are interest in --> One can acces this local by `treat2'
			* covariates: local of control variables  --> One can acces this local by `covariates'
			* gen: the name of the new matrix to be returned, stored in r(name)
		*/	
		
	/*
	The two groups are defined in the following local `h', where regression take subsamples if covariates
	take the value of 1 or 0.
	*/
foreach h in 0 1{

		* Defining the locals **************************************************
		
		local het_vars `varlist'
		* The local of dep_vars takes into account all variable outcomes defined in the rows section
		
		local regressors `treat1' `treat2' `covariates'
		* The local of regressors takes into account all rigth term of equation one.
	
		local dec `decimals'
		* The local dec takes the number of decimals in the table
		
		cap drop  pval1
		cap gen pval1 =.

		cap drop  pval2
		cap gen pval2 =.
		
		* Initialize matrices **************************************************
		
		mat control_mean	= J(`:word count `het_vars'',3,.) // Matrix for column 1 and 4: Control mean
		mat reg_count 		= J(`:word count `het_vars'',3,.) // Matrix for number of observations

		mat reg1 			= J(`:word count `het_vars'',3,.) // Matrix for columns 2 and 5: Transport subsidy
 		mat reg2 			= J(`:word count `het_vars'',3,.) // Matrix for columns 3 and 6: Workshop intervention

		mat stars1 			= J(`:word count `het_vars'',3,.) // Matrix for the legend of statistical significance reg1
 		mat stars2 			= J(`:word count `het_vars'',3,.) // Matrix for the legend of statistical significance reg2

		*mat pval 			= J(`:word count `het_vars'',3,.)

		mat cpval 			= J(`:word count `het_vars'',6,.) // Matrix for columns 7 and 8: p values of ttest linear hypotheses
 
		* Initializing row names ***********************************************
		
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

		local i = 1
		
		foreach y in `het_vars'{ 
		
			* Making sure no problems occur if the variable already exists
			cap drop `treat1'_h_*
			cap drop `treat2'_h_*
			
			*Generating vairables
			gen `treat1'_h_0 = `treat1'==1&`y'==0
			gen `treat1'_h_1 = `treat1'==1&`y'==1
			gen `treat2'_h_0 = `treat2'==1&`y'==0
			gen `treat2'_h_1 = `treat2'==1&`y'==1
		
 
			* inputting values into the matrix piece by piece ******************
			
			qui sum `outcome' if (treat_groupind == 5) & (`y'==`h') [aw=ed_weight]  // define the control group
			
			/*
			Defining the control mean for each variable using inverse frequency weights in the sample and if the 
			covariate is equal to 1 or 0
			*/
			
			* control mean -----------------------------------------------------
			
			mat control_mean[`i', 1] 	= r(mean) // Saving the value to the mean matrix
		

			cap sum bs_`outcome',d 
			capture confirm variable bs_`outcome'
			
			/*
			The previuos line of code is an excellent way of telling stata to do two different things if the condition is
			met. Thus, if the variable is the baseline outcome variable is in the data set, then run the regression model
			taking the baseline outcome variable as a covariates. Conversly, if the condition is not met (e.g. the baseline
			variable is not in the data set), then run the regression model without the baseline variable as covariate.
			
			IT REALLY IS A COOL WAY TO HANDLE THIS PROBLEM
			*/
					
			if !_rc {
				reg `outcome' bs_`outcome' `treat1'_h_0 `treat1'_h_1 `treat2'_h_0 `treat2'_h_1 `y' `covariates' [pw=ed_weight], cluster(cluster_id) 
			}
			else {
			 reg `outcome' `treat1'_h_0 `treat1'_h_1 `treat2'_h_0 `treat2'_h_1 `y' `covariates' [pw=ed_weight], cluster(cluster_id)
			}

			/*
			
			Regression structure:
			
			The estimation to estimate is the following:
			
			Y_ic = β_0 + β_1 x treat\_transport_{fic}|h==0 + β_1 x treat\_transport_{fic}|h==1 +
			       β_2 x treat\_workshop_{fic}|h=0 + β_2 x treat\_workshop_{fic}|h=1 + γ x spillover_{fic}] + 
				   α x y_{ic,pre} + δ x X_{ic0} + μ_{ic}
			
			
			The elements of the regression are:
				1. Dependent variable: Does not change within the programa. The option `outcome' sets the value of the
				   dependent variable for the whole steps.
				2. Pre treatment outcome variable: As explained above, capture confirm variable bs_`y' help to
				   determine if the baseline outcome variable should be included in the model.
				3. Regressors: Defined as the covariates input in the program plus the two branches of treatment;
				   1) transport subsidy or 2) workshop intervention.
				4. Regression weights: Given the sampling frequency of differente groups of the population
				   to be represented in the study sample, inverse weights help to estimate the ITT according to
				   the proportion of this subgroups in the sample. This is done by the option in brackets pw
				5. Standard cluster errors: As the randomization of the sample and treatments was done in two steps.
				   Firstly geographical zones and secondly within zones teatment randomization, the standar errors should
				   be cluster to allow correlation within geographical zones of individuals. This is done by the option
				   cluster(cluser_var)
			
			I eliminated two lines that were repeated below
			*/

			* count of regression ----------------------------------------------
			
			mat reg_count[`i', 1] 		= e(N)  // Saving the value to the observations matrix
			
			* P values and stars  ----------------------------------------------
			
			local p1 = (2 * ttail(e(df_r), abs(_b[`treat1'_h_`h']/_se[`treat1'_h_`h'])))
			/*
			Local p1 is not more than takinf the inverse normal function in order to find the
			p value assotiated with a t statistic given in each regression for coefficient
			`treat1'_h_`h'
			*/
			quiet replace pval1 =  `p1' if _n==`i'
			
			/*
			I set this parameter quietly so it does not prints so many lines in the results windows
			*/

			mat stars1[`i', 2] = 0 // make the first column of stars 0
			mat stars1[`i', 3] = 0 // make the first column of stars 0

			if (`p1' < .1) 		mat stars1[`i',1] = 1 // less than 10%?
			else 				mat stars1[`i',1] = 0 // if not, no stars
			if (`p1' < .05) 	mat stars1[`i',1] = 2 // less than 5%?
			if (`p1' < .01) 	mat stars1[`i',1] = 3 // less than 1%?
		
			local p2= (2 * ttail(e(df_r), abs(_b[`treat2'_h_`h']/_se[`treat2'_h_`h'])))
			* Local for pvalu associated with the workshop intervention
			
			quiet replace pval2 =  `p2' if _n==`i' 
			/*
			I set this parameter quietly so it does not prints so many lines in the results windows
			*/

			mat stars2[`i', 2] = 0 // make the first column of stars 0
			mat stars2[`i', 3] = 0 // make the first column of stars 0

			if (`p2' < .1) 		mat stars2[`i',1] = 1 // less than 10%?
			else                mat stars2[`i',1] = 0 // if not, no stars
			if (`p2' < .05) 	mat stars2[`i',1] = 2 // less than 5%?
			if (`p2' < .01) 	mat stars2[`i',1] = 3 // less than 1%?

			* Regression parameter estimates -----------------------------------
			
			mat reg1[`i',1] = _b[`treat1'_h_`h']
			mat reg1[`i',2] = _se[`treat1'_h_`h']

			mat reg2[`i',1] = _b[`treat2'_h_`h']
			mat reg2[`i',2] = _se[`treat2'_h_`h']
				
			* Linear hypotheses after estimation -------------------------------
			
			test  `treat1'_h_0 = `treat1'_h_1
			/*
			Test whether estimates are equal. Speciffically if the transport
			subsidies estimates are equal when each of the baseline variables
			are either 0 or 1
			*/
			mat cpval[`i',1] = r(p)	// Saving the p value of the t test.
			
			test  `treat2'_h_0 = `treat2'_h_1
			/*
			Test whether estimates are equal. Speciffically if the workshop
			intervention estimates are equal when each of the baseline variables
			are either 0 or 1
			*/
			mat cpval[`i',4] = r(p)	// Saving the p value of the t test.
	
			local i = `i' + 1 // Increasing the value of local `i' for the next loop iteration
		}
	 
		
	* Create q-values **********************************************************
	
	foreach p in 1 2{
		preserve 
		
		quietly keep pval`p'  `het_vars'  `regressors'   // Filtering the data set
		quietly keep if pval`p'!=.                       // Keep those observation that have values of pval1 or pval2 different to missing
	  		
		quietly sum pval`p'
		
		local totalpvals = r(N)   // Total number of nonmissing observations with pvalue

		quietly gen int original_sorting_order = _n
		quietly sort pval`p'
		quietly gen int rank = _n if pval`p'~=.
		
		gen bky06_qval = 1 if pval`p'~=.

		local qval = 1
		while `qval' > 0 {
		
			* First Stage ----------------------------------------------------------
			
			* Generate the adjusted first stage q level we are testing: q' = q/1+q
			local qval_adj = `qval'/(1+`qval')
			
			* Generate value q'*r/M
			quietly gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
			
			* Generate binary variable checking condition p(r) <= q'*r/M
			quietly gen reject_temp1 = (fdr_temp1>=pval`p') if pval`p'~=.
			
			* Generate variable containing p-value ranks for all p-values that meet above condition
			quietly gen reject_rank1 = reject_temp1*rank
			
			* Record the rank of the largest p-value that meets above condition
			quietly egen total_rejected1 = max(reject_rank1)

			* Second Stage ---------------------------------------------------------
			
			* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
			local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
			
			* Generate value q_2st*r/M
			quietly gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
			
			* Generate binary variable checking condition p(r) <= q_2st*r/M
			quietly gen reject_temp2 = (fdr_temp2>=pval`p') if pval`p'~=.
			
			* Generate variable containing p-value ranks for all p-values that meet above condition
			quietly gen reject_rank2 = reject_temp2*rank
			
			* Record the rank of the largest p-value that meets above condition
			quietly egen total_rejected2 = max(reject_rank2)
			

			* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
			quietly replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
			
			* Reduce q by 0.001 and repeat loop
			quietly drop fdr_temp* reject_temp* reject_rank* total_rejected*
			
			local qval = `qval' - .001
		}
	
		local qval = 1
		quietly gen bh95_qval = 1 if pval~=.

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

	* Merge matrices together to form larger matrix ****************************
	
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
