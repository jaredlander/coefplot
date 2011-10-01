## information on models
modelInfo <- function(model, ...)
{
    UseMethod("modelInfo", model)
}


modelInfo.lm <- function(model, shorten=TRUE, factors=NULL, only=NULL, ...)
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
    
    matchedVars <- buildFactorDF(modelFactorVars=factorVars, modelModel=model$model, modelCoefs=newList, shorten=shorten, factors=factors, only=only)    # figure out which variable belongs to each coefficient
    return(matchedVars)
    return(newList)
}
#modelInfo(model3)
#names(coef(model3))
#print("hi")
#print("bye")
