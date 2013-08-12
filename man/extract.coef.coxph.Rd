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
require(survival)
head(bladder)
blad1 <- coxph(Surv(stop, event) ~ rx + number + size + enum, data=bladder)
extract.coef(blad1)
}
\author{
  Nicholas Galasinao
}

