\name{getModelInfo.rxLogit}
\alias{getModelInfo.rxLogit}
\title{Model Information for rxLogit}
\usage{
  getModelInfo.rxLogit(...)
}
\arguments{
  \item{\dots}{Further arguments.  See
  \code{\link{getModelInfo.rxLinMod}} for more information
  on this function's arguments.}
}
\value{
  Information on the model
}
\description{
  Extracts and builds extensive information from rxLogit
  models
}
\details{
  Helper function for \code{\link{coefplot}}

  See \code{\link{getModelInfo.rxLinMod}} for more
  information on this function's arguments as it is simply
  a wrapper function.
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut*color, data=diamonds)
coefplot(model1)
}
\author{
  Jared P. Lander
}
\seealso{
  \code{\link{coefplot.lm}} \code{\link{coefplot}}
}

