\name{checkForIntercept}
\alias{checkForIntercept}
\title{checkForIntercept}
\usage{
  checkForIntercept(model, ...)
}
\arguments{
  \item{model}{Model to be checked for an intercept}

  \item{\dots}{Further Arguments}
}
\value{
  TRUE or FALSE (actually 0 or 1)
}
\description{
  Checks models for intercept terms
}
\details{
  Most models have a slot in the terms attribute for
  whether or not there is an intercept.  However, some
  models, like coxph, do not properly report.  So now this
  check will be done by object specific functions.
}
\examples{
require(survival)
head(bladder)
blad1 <- coxph(Surv(stop, event) ~ rx + number + size + enum, data=bladder)

data("tips", package="reshape2")
tips1 <- lm(tip ~ total_bill + sex, data=tips)
tips2 <- lm(tip ~ total_bill + sex - 1, data=tips)
checkForIntercept(tips1)
checkForIntercept(tips2)
}
\author{
  Jared P. Lander
}
\seealso{
  checkForIntercept.default, checkForIntercept.coxph
}

