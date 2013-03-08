\name{buildModelCI}
\alias{buildModelCI}
\title{buildModelCI}
\usage{
  buildModelCI(model, outerCI = 2, innerCI = 1,
    intercept = TRUE, numeric = FALSE,
    sort = c("natural", "magnitude", "alphabetical"),
    variables = NULL, newNames = NULL, decreasing = TRUE,
    name = NULL, interceptName = "(Intercept)", ...)
}
\arguments{
  \item{model}{A Fitted model such as from lm, glm}

  \item{innerCI}{How wide the inner confidence interval
  should be, normally 1 standard deviation.  If 0, then
  there will be no inner confidence interval.}

  \item{outerCI}{How wide the outer confidence interval
  should be, normally 2 standard deviations.  If 0, then
  there will be no outer confidence interval.}

  \item{sort}{Determines the sort order of the
  coefficients.  Possible values are c("natural",
  "magnitude", "alphabetical")}

  \item{decreasing}{logical; Whether the coefficients
  should be ascending or descending}

  \item{variables}{A character vector specifying which
  variables to keep.  Each individual variable has to be
  specfied, so individual levels of factors must be
  specified.  We are working on making this easier to
  implement, but this is the only option for now.}

  \item{newNames}{Named character vector of new names for
  coefficients}

  \item{numeric}{logical; If true and factors has exactly
  one value, then it is displayed in a horizontal graph
  with constinuous confidence bounds.; not used for now.}

  \item{intercept}{logical; Whether the Intercept
  coefficient should be plotted}

  \item{interceptName}{Specifies name of intercept it case
  it is not the default of "(Intercept").}

  \item{\dots}{See Details for information on
  \code{factors}, \code{only} and \code{shorten}}

  \item{name}{A name for the model, if NULL the call will
  be used}
}
\value{
  A \code{\link{data.frame}} listing coeffcients and
  confidence bands.
}
\description{
  Construct Confidence Interval Values
}
\details{
  Takes a model and builds a data.frame holding the
  coefficient value and the confidence interval values.
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
coefplot:::buildModelCI(model1)
coefplot(model1)
}
\author{
  Jared P. Lander
}
\seealso{
  \code{\link{coefplot}} \code{\link{multiplot}}
}

