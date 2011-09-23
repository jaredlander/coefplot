## information on models
modelInfo <- function(model, ...)
{
    UseMethod("modelInfo", model)
}

modelInfo.lm <- function(model)
{
    # get the model summary to easily get info out of it
    modelSummary <- summary(model)
    
    ## extract coefficients and standard errors
	coef <- modelSummary$coefficients[, 1]
	SE <- modelSummary$coefficients[, 2]		# gets standard error from summary
    
    varTypes <- attr(model$terms, "dataClasses")			## These are the types of the different variables
	factorVars <- names(varTypes[varTypes %in% c("factor", "other")])		## The variables that are factor
    
    return(factorVars)
}
