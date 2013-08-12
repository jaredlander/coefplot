\name{extract.coef.coxph}
\alias{extract.coef.coxph}
\title{extract.coef.coxph}
\usage{
  \method{extract.coef}{coxph} (model, ...)
}
\arguments{
  \item{model}{Model object to extract information from.}

  \item{\dots}{Further arguments}
}
\value{
  A \code{\link{data.frame}} containing the coefficient,
  the standard error and the variable name.
}
\description{
  Extract Coefficient Information from Models
}
\details{
  Gets the coefficient values and standard errors, and
  variable names from a model.
}
\examples{
require(ggplot2)
data(diamonds)
mod1 <- lm(price ~ carat + cut + x, data=diamonds)
\dontrun{extract.coef(mod1)}
}
\author{
  Nicholas Galasinao
}

