## information on models
#' @import stringr
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
	factorVars <- names(varTypes[varTypes %in% c("factor", "other")])    	## The variables that are factor

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
		matchedVars <- data.frame(Var=NA, Checkers=NA, Coef=NA, CoefShort=NA)
	}
    
	rm(varTypes); gc()		# do some memory cleanup
	
	list(coef=coef, SE=SE, factorVars=factorVars, factorVarsHuman=factorVars, factorCoefs=newList, matchedVars=matchedVars)				## return the coefs and SEs as a named list
}


## can deal with this in a very simliar way to lm
getModelInfo.rxLinMod <- function(model, shorten=TRUE, factors=NULL, only=NULL, ...)
{
	# get the model summary to easily get info out of it
    #modelSummary <- summary(model)[[1]]
    
    ## extract coefficients and standard errors
	coef <- model$coefficients#modelSummary$coefficients[, 1, drop=F]             #model$coefficients          # coefficients
	SE <- model$coef.std.error#modelSummary$coefficients[, 2, drop=F]               #model$coef.std.error		# gets standard error

    ## the special characters and their escaped equivalents
    specials <- c("!", "(", ")", "-", "=", ".")

    ## if they are reducing variables, get rid of them now
    if(!is.null(factors))
    {
        # take care of special characters
        factors <- subSpecials(factors, specialChars=specials)[[1]]

        # make a pipe seperated character of factors to keep
        toKeep <- paste(factors, collapse="|")

        ## if they are only keeping the factor itself, and not interactions
        if(identical(only, TRUE))
        {
            # just check for when that variable is the only one
            theString <- paste("^(", toKeep, ")=[^, ]*?$", sep="")
        }else
        {
            # as long as that variable is in there
            theString <- paste("(^|, | for )(", toKeep, ")=", sep="")
        }
        
        # the ones we are going to keep
        theKeepers <- grep(theString, rownames(coef))

        # take just the coefs and SEs that were specified
        coef <- coef[theKeepers, , drop=FALSE]
        SE <- SE[theKeepers, , drop=FALSE]
    }

    ## figure out which variables are factors
    # get coefficient names
    coefNames <- rownames(coef)

    # get the variables put into the formula and clean them up
    theTerms <- paste(rxModel6$formula)[[3]]
    theTerms <- gsub("\\*|\\+", "|", theTerms)
    theTerms <- gsub(" ", "", theTerms)

    # get those variables out of the coefficient names
    # these are the factor variables thanks to the equal sign in the regular expression
    factorVars <- unlist(str_extract_all(string=coefNames, pattern=paste("(^|, | for )(", theTerms, ")=", sep="")))

    # strip out spaces, for, commas and equals and get unique values
    factorVars <- unique(gsub("(^ for )|(^, )|(=$)", "", factorVars))

    # build matched df
 
    if(length(factorVars) > 0)
    {
        matchedVars <- rxVarMatcher(modelFactorVars=factorVars, modelCoefNames=coefNames, shorten=shorten)
    }else
    {
		newList <- NA
		matchedVars <- data.frame(Var=NA, Checkers=NA, Coef=NA, CoefShort=NA)
	}

 
    return(list(coef=coef, SE=SE, factorVars=factorVars, factorCoefs=coefNames, matchedVars=matchedVars))    			## return the coefs and SEs as a named list
}