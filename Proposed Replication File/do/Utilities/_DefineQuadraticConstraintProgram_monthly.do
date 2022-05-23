* ImposeQuadraticConstraints

** This do-file defines the program to impose the quadratic constraints.  Then you call it...

capture program drop SimonConstraints

program SimonConstraints
	args p q start matname

	matrix `matname' = J(`p', `q', 0)
	local m = `q' + 1

	forvalues i = 1/`p' {
	
		local Target1 	= `i' - 1 + `start'
		local Target2  	= `i' - 1 + `start' + 1
		local Target3 	= `i' - 1 + `start' + 2
		local Target4  	= `i' - 1 + `start' + 3
		
		matrix `matname'[`i', `Target1'] 	= -1
		matrix `matname'[`i', `Target2'] 	= 3
		matrix `matname'[`i', `Target3'] 	= -3
		matrix `matname'[`i', `Target4'] 	= 1
		
		
	}

end

capture program drop DefineQuadraticConstraintsM

program DefineQuadraticConstraintsM

	args 	MyOutcomeVariable

	* First, figure out how many controls there are...

	quietly 	reg `MyOutcomeVariable' SQ_month_*
	global 		ControlCount 		= e(rank)

	* Second, figure out the total number of variables we will need...

	quietly 	reg `MyOutcomeVariable' SQ_tg_*_* SQ_month_*
	global		ColumnsRequired = e(rank) + 1

	* Third, count each of the separate treatment dummy sets...

	global 	CurrentRestrictionCount = 0

	forvalues TreatmentLabel = 1/2 ///
		{
		
		if `TreatmentLabel' != 5 ///
		{
		quietly reg `MyOutcomeVariable' SQ_tg_`TreatmentLabel'_*_* SQ_month_*
		
		global 		RestrictionSize 	= e(rank) - $ControlCount- 3
		
		global 		StartingVariable 	= $CurrentRestrictionCount + 1 
		
		SimonConstraints $RestrictionSize $ColumnsRequired $StartingVariable 	CB`TreatmentLabel'
		
		display "$RestrictionSize $ColumnsRequired $StartingVariable 	ConstraintBlock`TreatmentLabel'"

		global 		CurrentRestrictionCount = $CurrentRestrictionCount + e(rank) - $ControlCount
		}
		}
	

	matrix MyBigConstraint = (CB1 \ CB2 )

end

exit

