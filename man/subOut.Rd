\name{subOut}
\alias{subOut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Sub special characters out of a character vector.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Converts each of the special characters to their equivalents in each element of a single vectorvectors.
}
\usage{
subOut(toAlter, specialChars = c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*"), modChars = c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{toAlter}{
%%     ~~Describe \code{toAlter} here~~
Character vector that will be altered by subbing the special characters with their escaped equivalents
}
  \item{specialChars}{
%%     ~~Describe \code{specialChars} here~~
The characters to be subbed out
}
  \item{modChars}{
%%     ~~Describe \code{modChars} here~~
The escaped versions 
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
function (toAlter, specialChars = c("\\!", "\\(", "\\)", "\\-", 
    "\\=", "\\*"), modChars = c("\\\\!", "\\\\(", "\\\\)", "\\\\-", 
    "\\\\=", "\\\\*")) 
{
    if (length(specialChars) != length(modChars)) {
        stop("specialChars and modChars must be the same length", 
            call. = FALSE)
    }
    for (i in 1:length(specialChars)) {
        toAlter <- gsub(specialChars[i], modChars[i], toAlter)
    }
    return(toAlter)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
