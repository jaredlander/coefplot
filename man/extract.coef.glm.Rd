\name{extract.coef.glm}
\alias{extract.coef.glm}
\title{extract.coef.glm}
\usage{
  \method{extract.coef}{glm} (model, ...)
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
  Extract Coefficient Information from glm Models
}
\details{
  Gets the coefficient values and standard errors, and
  variable names from a glm model.
}
\examples{
require(ggplot2)
data(diamonds)
mod2 <- glm(price > 10000 ~ carat + cut + x, data=diamonds, family=binomial(link="logit"))
extract.coef(mod2)
}
\author{
  Jared P. Lander
}

