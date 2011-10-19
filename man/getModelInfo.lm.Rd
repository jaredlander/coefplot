\name{getModelInfo.lm}
\alias{getModelInfo.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getModelInfo.lm(model, shorten = TRUE, factors = NULL, only = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{shorten}{
%%     ~~Describe \code{shorten} here~~
}
  \item{factors}{
%%     ~~Describe \code{factors} here~~
}
  \item{only}{
%%     ~~Describe \code{only} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(model, shorten=TRUE, factors=NULL, only=NULL, ...)
{
    # get the model summary to easily get info out of it
    modelSummary <- summary(model)
    
    ## extract coefficients and standard errors
	coef <- modelSummary$coefficients[, 1]
	SE <- modelSummary$coefficients[, 2]		# gets standard error from summary
    
    varTypes <- attr(model$terms, "dataClasses")			## These are the types of the different variables
	factorVars <- names(varTypes[varTypes \%in\% c("factor", "other")])    	## The variables that are factor

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
			remainingFactors <- which(names(coef) \%in\% matchedVars$Coef)
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
