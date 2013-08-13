\name{checkForIntercept.coxph}
\alias{checkForIntercept.coxph}
\title{checkForIntercept.coxph}
\usage{
  \method{checkForIntercept}{coxph} (model, ...)
}
\arguments{
  \item{model}{Model to be checked for an intercept}
}
\value{
  TRUE or FALSE (actually 0 or 1)
}
\description{
  coxph method
}
\details{
  coxph models should never be considered to have an
  intercept even though the terms element always reports an
  intercept.  So this function just returns 0.
}
\author{
  Jared P. Lander
}
\seealso{
  checkForIntercept
}

