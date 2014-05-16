\name{extract.coef.lm}
\alias{extract.coef.lm}
\title{extract.coef.lm}
\usage{
\method{extract.coef}{lm}(model, ...)
}
\arguments{
  \item{\dots}{Further arguments}

  \item{model}{Model object to extract information from.}

  \item{...}{Further arguments}
}
\value{
A \code{\link{data.frame}} containing the coefficient, the
standard error and the variable name.
}
\description{
Extract Coefficient Information from lm Models
}
\details{
Gets the coefficient values and standard errors, and
variable names from an lm model.
}
\examples{
require(ggplot2)
data(diamonds)
mod1 <- lm(price ~ carat + cut + x, data=diamonds)
extract.coef(mod1)
}
\author{
Jared P. Lander
}

