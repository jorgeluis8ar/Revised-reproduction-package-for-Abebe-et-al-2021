* custom_regression 

cap program drop attrition_regression_table
program define attrition_regression_table
	clear mata 
	syntax varlist, COVARIATES(varlist) TREATMENTS(varlist) FILENAME(name)

	* We only want one dependent variable in this regression *******************
	if (`:word count `varlist'' > 1) {
		display "Error: only one variable allowed before comma"
		exit
	}

	* Renaming the local for the dependent variable ****************************
	local y `varlist'
	local regressors `treatments'  `covariates' 
	
	* Get number of regressors in order to make table **************************
	local M = `:word count `regressors'' 

	* Initialize the regression matrix *****************************************
	/* 	We add two to the height in order to fit in the P value of the F test 
		and the count 
	*/
	mat regmat = J(`M', 4, .)
 	mat rownames regmat = `regressors' 

 	mat stars = J(`M', 4, .)
 	mat rownames stars = `regressors' 
	
	mat stats = J(2, 4, .)
	mat rownames stats = "P-value of F-test" "N"
	
	* First two columns ********************************************************
	
	* Run the regression *******************************************************
//	svy: reg `y' `treatments'
	reg `y' `treatments'   ,   cluster( cluster_id)
	 	
	local i = 1
 	foreach var in `treatments' {
 		mat regmat[`i', 1] = _b[`var']
 		mat regmat[`i', 2] = _se[`var']

 		local p = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))

		mat stars[`i', 1] = 0 // make the first column of stars 0

			if (`p' < .1) 		mat stars[`i',2] = 1 // less than 10%?
				else mat stars[`i',2] = 0 // if not, no stars
			if (`p' < .05) 	mat stars[`i',2] = 2 // less than 5%?
			if (`p' < .01) 	mat stars[`i',2] = 3 // less than 1%?

 		local ++i
 	}

	
				display "hulme"
	
	*local i = 2 
 	foreach var in `covariates' {
 		mat regmat[`i', 1] =.
 		mat regmat[`i', 2] =.
		mat stars[`i', 1] = 0 // make the first column of stars 0
		mat stars[`i', 2] = 0 // make the second column of stars 0
 		local ++i
 	}


	* Perform the F test *******************************************************
 	test `treatments'
 	mat stats[1, 1] = r(p) // input value 

 	* Input the count **********************************************************
 	mat stats[2, 1] = e(N)
	****************************************************************************	

	
	* Last two columns *********************************************************
	* Run the regression *******************************************************
// 	 svy: reg `y' `regressors' 	
 	 reg `y' `regressors'  ,  cluster( cluster_id)

 	* Input all the values individually ****************************************
 	local i = 1
 	foreach var in `regressors' {
 		mat regmat[`i', 3] = _b[`var']
 		mat regmat[`i', 4] = _se[`var']

 		local p = (2 * ttail(e(df_r), abs(_b[`var']/_se[`var'])))

		mat stars[`i', 3] = 0 // make the first column of stars 0
		
			if (`p' < .1) 		mat stars[`i',4] = 1 // less than 10%?
				else mat stars[`i',4] = 0 // if not, no stars
			if (`p' < .05) 	mat stars[`i',4] = 2 // less than 5%?
			if (`p' < .01) 	mat stars[`i',4] = 3 // less than 1%?

 		local ++i
 	}
	

	* Perform the F test *******************************************************
 	test `regressors'
 	mat stats[1, 3] = r(p) // input value 


 	* Input the count **********************************************************
 	mat stats[2, 3] = e(N)
	****************************************************************************	
	

 	* Build a string for the hlines() command **********************************
 	/* 	We need to build a string of 0s and 1s to tell frmttable where to put 
		the horizontal lines. We do this using a local that stores a string. 
	*/

	
	* Define the lines for formattable *****************************************

	local lines 10001
	local t = `M' -1
	forvalues i = 1/`t' {
		local lines `lines' 0
	}
	local lines `lines'  101

	
	matrix list regmat
	matrix list stars

	
 	* Assemble the ouput *******************************************************
 	frmttable, statmat(regmat) sdec(3) varlabels   ///
	annotate(stars) asymbol(*,**,***)
		
		
 
//	frmttable, statmat(second_regression) sdec(3) ///
//		annotate(stars) asymbol(*,**,***) merge

 	frmttable, statmat(stats) sdec(4 \ 0) append

	frmttable using "tables/`filename'", ///	
		tex ///
		fragment ///
		nocenter ///
		varlabels ///
		replace ///
		ctitle("", "\underline{`:var label `y''}", "", "" \ ///
		"", "Only Treatment", "", "All Covariates", "" \ ///
		"Dependent Variable: No-response or refused", "Coeff", "Std. error", "Coeff", "Std. error" \ ///
		"", "(1)", "(2)", "(3)", "(4)") ///
		multicol(1, 2, 4; 2, 2, 2;, 2, 4, 2) ///
		hlines(`lines') 

end


********************************************************************************
