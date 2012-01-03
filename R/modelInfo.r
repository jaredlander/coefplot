## information on models
#' Model Information
#'
#' Extracts and builds extensive information from models
#'
#' Helper function for \code{\link{coefplot}}
#' @author Jared P. Lander
#' @seealso \code{\link{coefplot.lm}}
#' @param model The fitted model with coefficients to be plotted
#' @param \dots Further arguments such as shorten, only and factors
#' @import stringr
#' @rdname getModelInfo
#' @return Information on the model
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
#'
getModelInfo <- function(model, ...)
{
    UseMethod("getModelInfo", model)
}


## Builds all of the info necessary for building the graph
# param model (lm object) the model to be plotted
# param shorten (logical or character vector) logical if all or none of the factors should be shortened, if character then only the variables listed will be shortened
# param factors (character vector) a list of factors to include, if NULL all of them will be included
# param only (logical) if TRUE then only the specified factors will be computed, otherwise the included factors and their interactions will be computed
# param \dots other options
# return Information on the model
#' Model Information
#'
#' Extracts and builds extensive information from lm and glm models
#'
#' Helper function for \code{\link{coefplot}}
#' @author Jared P. Lander
#' @seealso \code{\link{coefplot.lm}}
#' @param model The fitted model with coefficients to be plotted
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @param \dots Further arguments
#' @import stringr
#' @rdname getModelInfo.lm
## @method getModelInfo lm
#' @S3method getModelInfo lm
#' @return Information on the model
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
#'
getModelInfo.lm <- function(model, shorten=TRUE, factors=NULL, only=NULL, ...)
{
    # get the model summary to easily get info out of it
    modelSummary <- summary(model)

    ## extract coefficients and standard errors
	coef <- modelSummary$coefficients[, 1]
	SE <- modelSummary$coefficients[, 2]		# gets standard error from summary

    ## strategy for narrowing down variables before hand
    # narrow variables that contain the name
    # do normal processing
    # then narrow based on matchedVars$Checkers
    
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
#' Model Information for rxLinMod
#'
#' Extracts and builds extensive information from rxLinMod models
#'
#' Helper function for \code{\link{coefplot}}
#' @author Jared P. Lander
#' @seealso \code{\link{coefplot.lm}} \code{\link{coefplot}}
#' @param model The fitted model with coefficients to be plotted
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @param \dots Further arguments
#' @import stringr
#' @rdname getModelInfo.rxLinMod
## @method getModelInfo rxLinMod
#' @S3method getModelInfo rxLinMod
#' @return Information on the model
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
#' 
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
    theTerms <- paste(model$formula)[[3]]
    theTerms <- gsub("\\*|\\+", "|", theTerms)
    theTerms <- gsub(" ", "", theTerms)
    theTerms <- gsub("F\\(([A-Za-z0-9]+)\\)", "F_\\1", theTerms)

    # get those variables out of the coefficient names
    # these are the factor variables thanks to the equal sign in the regular expression
    factorVars <- unlist(str_extract_all(string=coefNames, pattern=paste("(^|, | for )(", theTerms, ")=", sep="")))
    # the function below depends on R 2.14 so it cannot be used for now to preserve backward compatability
    #theReg <- gregexpr(pattern=paste("(^|, | for )(", theTerms, ")=", sep=""), text=coefNames)
    #factorVars <- unlist(regmatches(x=coefNames, m=theReg))

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


## can deal with this in a very simliar way to lm
#' Model Information for rxLogit
#'
#' Extracts and builds extensive information from rxLogit models
#'
#' Helper function for \code{\link{coefplot}}
#' @author Jared P. Lander
#' @seealso \code{\link{coefplot.lm}} \code{\link{coefplot}}
#' @param model The fitted model with coefficients to be plotted
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @param \dots Further arguments
#' @import stringr
#' @rdname getModelInfo.rxLogit
## @method getModelInfo rxLogit
#' @S3method getModelInfo rxLogit
#' @return Information on the model
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
#'
getModelInfo.rxLogit <- function(...)
{
    getModelInfo.rxLinMod(...)
}