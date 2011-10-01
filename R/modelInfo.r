## information on models
getModelInfo <- function(model, ...)
{
    UseMethod("modelInfo", model)
}


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
	
	if(length(factorVars) > 0)
	{		
		matchedVars <- buildFactorDF(modelFactorVars=factorVars, modelModel=model$model, modelCoefs=newList, shorten=shorten, factors=factors, only=only)    # figure out which variable belongs to each coefficient
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
