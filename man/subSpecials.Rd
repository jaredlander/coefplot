\name{subSpecials}
\alias{subSpecials}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
subSpecials(..., specialChars = c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*"), modChars = c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*"), simplify = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{specialChars}{
%%     ~~Describe \code{specialChars} here~~
}
  \item{modChars}{
%%     ~~Describe \code{modChars} here~~
}
  \item{simplify}{
%%     ~~Describe \code{simplify} here~~
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
function (..., specialChars = c("\\!", "\\(", "\\)", "\\-", "\\=", 
    "\\*"), modChars = c("\\\\!", "\\\\(", "\\\\)", "\\\\-", 
    "\\\\=", "\\\\*"), simplify = FALSE) 
{
    if (length(specialChars) != length(modChars)) {
        stop("specialChars and modChars must be the same length", 
            call. = FALSE)
    }
    return(llply(list(...), subOut, specialChars = specialChars, 
        modChars))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
