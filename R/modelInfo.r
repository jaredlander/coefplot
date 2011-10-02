## information on models
getModelInfo <- function(model, ...)
{
    UseMethod("getModelInfo", model)
}


## Builds all of the info necessary for building the graph
## @model (lm object) the model to be plotted
## @shorten (logical or character vector) logical if all or none of the factors should be shortened, if character then only the variables listed will be shortened
## @factors (character vector) a list of factors to include, if NULL all of them will be included
## @only (logical) if TRUE then only the specified factors will be computed, otherwise the included factors and their interactions will be computed
## @... other options
getModelInfo.lm <- function(model, shorten=TRUE, factors=NULL, only=NULL, ...)
{
    # get the model summary to easily get info out of it
    modelSummary <- summary(model)
    
    ## extract coefficients and standard errors
	coef <- modelSummary$coefficients[, 1]
	SE <- modelSummary$coefficients[, 2]		# gets standard error from summary
    
    varTypes <- attr(model$terms, "dataClasses")			## These are the types of the different variables
	factorVars <- names(varTypes[varTypes %in% c("factor", "other")])		## The variables that are factor

	# store the names of the coefficients
    newList <- names(coef)    	## names of the coefficients
	
	## if there are some factor variables
	if(length(factorVars) > 0)
	{
		# figure out which variable belongs to each coefficient
		# passing just one row doesn't work
		matchedVars <- buildFactorDF(modelFactorVars=factorVars, modelModel=model$model, modelCoefs=newList, shorten=shorten, factors=factors, only=only)
		
		if(!is.null(factors))
		{
			## since some factors are not included they must also be removed from coef's and SE's
			remainingFactors <- which(names(coef) %in% matchedVars$Coef)
			coef <- coef[remainingFactors]
			SE <- SE[remainingFactors]
			newList <- newList[remainingFactors]
			rm(remainingFactors); gc()
		}
	}else
	{
		newList <- NA
		matchedVars <- data.frame(Var=NA, Coef=NA, Coefshort=NA)
	}
    
	rm(varTypes); gc()		# do some memory cleanup
	
	list(coef=coef, SE=SE, factorVars=factorVars, factorVarsHuman=factorVars, factorCoefs=newList, matchedVars=matchedVars)				## return the coefs and SEs as a named list
}
#modelInfo(model3)
#names(coef(model3))
#print("hi")
#print("bye")
