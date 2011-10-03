\name{buildFactorDF}
\alias{buildFactorDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildFactorDF(modelFactorVars, modelModel, modelCoefs, shorten = TRUE, factors = NULL, only = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelFactorVars}{
%%     ~~Describe \code{modelFactorVars} here~~
}
  \item{modelModel}{
%%     ~~Describe \code{modelModel} here~~
}
  \item{modelCoefs}{
%%     ~~Describe \code{modelCoefs} here~~
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
function (modelFactorVars, modelModel, modelCoefs, shorten = TRUE, 
    factors = NULL, only = NULL) 
{
    varDFTemp <- adply(modelFactorVars, 1, function(x, modelD) {
        expand.grid(x, extractLevels(x, modelD), stringsAsFactors = FALSE)
    }, modelModel)
    names(varDFTemp)[2:3] <- c("Var", "Pivot")
    varDF <- expand.grid(varDFTemp$Pivot, modelCoefs, stringsAsFactors = FALSE)
    names(varDF)[1:2] <- c("Pivot", "Coef")
    varDF <- join(varDF, varDFTemp, by = "Pivot")
    rm(varDFTemp)
    gc()
    varDF$PivotAlter <- varDF$Pivot
    varDF$VarAlter <- varDF$Var
    specials <- c("\\!", "\\(", "\\)", "\\-", "\\=")
    specialsSub <- c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=")
    alterList <- subSpecials(varDF$VarAlter, varDF$PivotAlter, 
        specialChars = specials, modChars = specialsSub)
    varDF$VarAlter <- alterList[[1]]
    varDF$PivotAlter <- alterList[[2]]
    rm(alterList)
    gc()
    varDF$Valid <- NA
    varDF$CoefShort <- NA
    varDF <- ddply(.data = varDF, .variables = "PivotAlter", 
        .fun = function(DF) {
            DF$Valid = regexpr(unique(DF$PivotAlter), DF$Coef, 
                ignore.case = FALSE)
            return(DF)
        })
    varDF <- varDF[varDF$Valid > 0, ]
    varDF$VarCheck <- varDF$VarAlter
    if (identical(class(shorten), "character")) {
        varDF[!varDF$Var \%in\% shorten, "VarAlter"] <- ""
    }
    varDF <- ddply(varDF, .(Coef), function(vect, namer, checker, 
        collapse, keepers) {
        vect$Subbers <- paste(vect[, c(namer)], collapse = collapse)
        vect$Checkers <- paste(vect[, c(checker)], collapse = collapse)
        return(vect[1, keepers])
    }, namer = "VarAlter", checker = "VarCheck", collapse = "|", 
        keepers = c("Var", "Coef", "Subbers", "Checkers", "CoefShort"))
    if (!is.null(factors)) {
        theCheckers <- strsplit(x = varDF$Checkers, split = "|", 
            fixed = TRUE)
        if (identical(only, TRUE)) {
            varDF <- varDF[varDF$Checkers \%in\% factors, ]
        }
        else {
            theKeepers <- laply(theCheckers, function(x, toCheck) {
                any(x \%in\% toCheck)
            }, toCheck = factors)
            varDF <- varDF[theKeepers, ]
            rm(theKeepers)
            gc()
        }
        rm(theCheckers)
        gc()
    }
    if (identical(shorten, FALSE)) {
        varDF$CoefShort <- varDF$Coef
        return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
    }
    varDF <- ddply(varDF, .(Subbers), function(DF) {
        DF$CoefShort <- gsub(unique(DF$Subbers), "", DF$Coef)
        return(DF)
    })
    return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
