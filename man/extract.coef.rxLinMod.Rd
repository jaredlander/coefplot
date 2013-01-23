\name{extract.coef.rxLinMod}
\alias{extract.coef.rxLinMod}
\title{extract.coef.rxLinMod}
\usage{
  \method{extract.coef}{rxLinMod} (model, ...)
}
\arguments{
  \item{\dots}{Further arguments}

  \item{model}{Model object to extract information from.}
}
\value{
  A \code{\link{data.frame}} containing the coefficient,
  the standard error and the variable name.
}
\description{
  Extract Coefficient Information from rxLinMod Models
}
\details{
  Gets the coefficient values and standard errors, and
  variable names from an rxLinMod model.
}
\examples{
\dontrun{
require(ggplot2)
data(diamonds)
mod3 <- rxLinMod(price ~ carat + cut + x, data=diamonds)
extract.coef(mod3)
}
}
\author{
  Jared P. Lander
}

