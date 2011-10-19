\name{coefplot}
\alias{coefplot}
\alias{coefplot.lm}
\alias{coefplot.rxLinMod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Coefficient Plot
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coefplot(model, ...)
coefplot.lm(model, title = "Coefficient Plot", xlab = "Value", ylab = "Coefficient", innerCI = 1, outerCI = 2, lwdInner = 1, lwdOuter = 0, color = "blue", cex = 0.8, textAngle = 0, numberAngle = 0, zeroColor = "grey", zeroLWD = 1, zeroType = 2, facet = FALSE, scales = "free", sort = "natural", decreasing = FALSE, numeric = FALSE, fillColor = "grey", alpha = 1/2, horizontal = FALSE, intercept = TRUE, plot = TRUE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
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
function(model, ...)
{
    UseMethod(generic="coefplot")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
