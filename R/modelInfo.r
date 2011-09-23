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

    # store the names of the coefficients
    newList <- names(coef)    	## names of the coefficients
    
    matchedVars <- buildFactorDF(modelFactorVars=factorVars, modelModel=model$model, modelCoefs=newList)    # figure out which variable belongs to each coefficient
    return(matchedVars)
    return(newList)
}
modelInfo(model3)
names(coef(model3))
print("hi")
print("bye")
buildFactorDF <- function(modelFactorVars, modelModel, modelCoefs)
{
    varDFTemp <- adply(modelFactorVars, 1, function(x, modelD) { expand.grid(x, extractLevels(x, modelD), stringsAsFactors=FALSE) }, modelModel)	## Build a frame of the variables and the coefficient names for the factor variables
	names(varDFTemp)[2:3] <- c("Var", "Pivot")		## give good names to the frame
	#varDF$Valid<- NA
	return(varDFTemp)
	varDF <- expand.grid(varDFTemp$Pivot, modelCoefs)
	names(varDF)[1:2] <- c("Pivot", "Coef")		## give good names to the frame
	varDF <- join(varDF, varDFTemp, by="Pivot")
	
	
	varDF$PivotAlter <- gsub("[!.()*=-]", "99", varDF$Pivot)						## alter punctuation in pattern also
	varDF$VarAlter <- gsub("[!.()*=-]", "99", varDF$Var)						## alter punctuation in pattern also
	varDF$CoefAlter <- gsub("[!.()*=-]", "99", varDF$Coef)					## alter punctuation in thing to be replaced also
	varDF$Valid<- NA
	
	varDF$CoefShort <- NA
	#varDF$CoefShortTemp <- NA
	
	for(i in 1:nrow(varDF))
	{
		varDF$Valid[i] <- with(varDF, regexpr(paste("", PivotAlter[i], "", sep=""), CoefAlter[i], ignore.case=FALSE))
	}
	
	varDF <- varDF[varDF$Valid > 0, ]
	
	varDF <- ddply(varDF, .(Coef), function(vect, namer, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); return(vect[1, keepers]) }, namer="Var", collapse="|", keepers=c("Var", "Coef", "Subbers", "CoefAlter", "CoefShort"))
	varDF$SubbersAlter <- gsub("[!.()=-]", "99", varDF$Subbers)					## alter punctuation in thing to be replaced also
	
	#subbers <- strsplit(varDF$VarAlter, ":", fixed=TRUE)		## if there are interaction terms split them up
	
	for(i in 1:nrow(varDF))
	{
		varDF$CoefShort[i] <- gsub(paste(varDF$SubbersAlter[i], collapse="|"), "", varDF$CoefAlter[i])
	}
	
	varDF$CoefBack<- gsub(":", ":|", varDF$CoefShort)
	
	for(i in 1:nrow(varDF))
	{
		varDF$Var[i] <- gsub(paste(varDF$CoefBack[i], "$", sep=""), ":", varDF$Coef[i])
	}
	
	varDF$Var <- gsub(":$", "", varDF$Var)
	
	return(varDF[, c("Var", "Coef", "CoefShort")])
}


## get the levels of factor variables
## @varName:  the variable to pay attention to
## @modelModel:  the model of interest
extractLevels <- function(varName, modelModel)
{
    # put the variable name in front of the level
    paste(varName, levels(factor(modelModel[[varName]])), sep="")
}