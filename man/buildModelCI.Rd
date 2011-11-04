\name{buildModelCI}
\alias{buildModelCI}
\title{Build data.frame for plotting}
\usage{
  buildModelCI(model, outerCI = 2, innerCI = 1, intercept =
  TRUE, numeric = FALSE, sort = "natural", decreasing =
  TRUE, name = NULL, ...)
}
\arguments{
  \item{model}{The fitted model to build information on}

  \item{innerCI}{How wide the inner confidence interval
  should be, normally 1 standard deviation.  If 0, then
  there will be no inner confidence interval.}

  \item{outerCI}{How wide the outer confidence interval
  should be, normally 2 standard deviations.  If 0, then
  there will be no outer confidence interval.}

  \item{sort}{Determines the sort order of the
  coefficients.  Possible values are c("natural", "normal",
  "magnitude", "size", "alphabetical")}

  \item{decreasing}{logical; Whether the coefficients
  should be ascending or descending}

  \item{numeric}{logical; If true and factors has exactly
  one value, then it is displayed in a horizontal graph
  with constinuous confidence bounds.}

  \item{intercept}{logical; Whether the Intercept
  coefficient should be plotted}

  \item{\dots}{See Details for information on
  \code{factors}, \code{only} and \code{shorten}}

  \item{name}{A name for the model, if NULL the call will
  be used}
}
\value{
  Otherwise a \code{\link{data.frame}} listing coeffcients
  and confidence bands is returned.
}
\description{
  Builds a data.frame that is appropriate for plotting
  coefficients
}
\details{
  This is the workhorse for coefplot, it get's the data all
  prepared

  \code{factors} Vector of factor variables that will be
  the only ones shown

  \code{only} logical; If factors has a value this
  determines how interactions are treated.  True means just
  that variable will be shown and not its interactions.
  False means interactions will be included.

  \code{shorten} logical or character; If \code{FALSE} then
  coefficients for factor levels will include their
  variable name.  If \code{TRUE} coefficients for factor
  levels will be stripped of their variable names.  If a
  character vector of variables only coefficients for
  factor levels associated with those variables will the
  variable names stripped.
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
model2 <- lm(price ~ carat, data=diamonds)
model3 <- lm(price ~ carat + cut + color, data=diamonds)
coefplot:::buildModelCI(model1)
#coefplot(model1)
#coefplot(model2)
#coefplot(model3)
#coefplot(model3, factors="cut")
#coefplot(model3, factors="cut", numeric=T)
#coefplot(model3, shorten="cut")
}
\author{
  Jared P. Lander www.jaredlander.com
}
\seealso{
  \code{\link{coefplot}}
}

