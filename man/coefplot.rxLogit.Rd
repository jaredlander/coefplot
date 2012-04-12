\name{coefplot.rxLogit}
\alias{coefplot.rxLogit}
\title{Dotplot for rxLogit coefficients}
\usage{
  \method{coefplot}{rxLogit} (...)
}
\arguments{
  \item{\dots}{All arguments are passed on to
  \code{\link{coefplot.lm}}.  Please see that function for
  argument information.}
}
\value{
  A ggplot object.  See \code{\link{coefplot.lm}} for more
  information.
}
\description{
  A graphical display of the coefficients and standard
  errors from a fitted rxLogit model
}
\details{
  \code{\link{coefplot}} is the S3 generic method for
  plotting the coefficients from a fitted model.

  For more information on this function and it's arguments
  see \code{\link{coefplot.lm}}
}
\examples{
# See coefplot.lm for examples
}
\author{
  Jared P. Lander www.jaredlander.com
}

