\name{coefplot}
\alias{coefplot}
\alias{coefplot.lm}
\alias{coefplot.rxLinMod}
\title{Dot plot for model coefficients}
\usage{
  coefplot(model, ...)
}
\arguments{
  \item{model}{The model we are graphing}

  \item{title}{The name of the plot, if NULL then no name
  is given}

  \item{xlab}{The x label}

  \item{ylab}{The y label}

  \item{innerCI}{How wide the inner confidence interval
  should be, normally 1 standard deviation.  If 0, then
  there will be no inner confidence interval.}

  \item{outerCI}{How wide the outer confidence interval
  should be, normally 2 standard deviations.  If 0, then
  there will be no outer confidence interval.}

  \item{lwdInner}{The thickness of the inner confidence
  interval}

  \item{lwdouter}{The thickness of the outer confidence
  interval}

  \item{color}{The color of the points and lines}

  \item{cex}{The text size multiplier, currently not used}

  \item{textAngle}{The angle for the coefficient labels, 0
  is horizontal}

  \item{numericAngle}{The angle for the value labels, 0 is
  horizontal}

  \item{zeroColor}{The color of the line indicating 0}

  \item{zeroLWD}{The thickness of the 0 line}

  \item{zeroType}{The type of 0 line, 0 will mean no line}

  \item{facet}{logical; If the coefficients should be
  faceted by the variables, numeric coefficients (including
  the intercept) will be one facet}

  \item{scales}{The way the axes should be treated in a
  faceted plot.  Can be c("fixed", "free", "free_x",
  "free_y")}

  \item{intercept}{logical; Whether the Intercept
  coefficient should be plotted}

  \item{plot}{logical; If the plot should be drawn, if
  false then a data.frame of the values will be returned}

  \item{\dots}{other arguments}

  \item{factors}{Vector of factor variables that will be
  the only ones shown}

  \item{only}{logical; If factors has a value this
  determines how interactions are treated.  True means just
  that variable will be shown and not its interactions.
  False means interactions will be included.}

  \item{shorten}{logical or character; If \code{FALSE} then
  coefficients for factor levels will include their
  variable name.  If \code{TRUE} coefficients for factor
  levels will be stripped of their variable names.  If a
  character vector of variables only coefficients for
  factor levels associated with those variables will the
  variable names stripped.}
}
\value{
  If \code{plot} is \{TRUE} then a \code{\link{ggplot}}
  object is returned.  Otherwise a \code{\link{data.frame}}
  listing coeffcients and confidence bands is returned.
}
\description{
  A graphical display of the coefficients and standard
  errors from a fitted model
}
\details{
  \code{coefplot} is the S3 generic method for plotting the
  coefficients from a fitted model.

  This can be extended with new methods for other types of
  models not currently available.

  A future iteration of \code{coefplot.glm} will allo for
  plotting the coefficients on the transformed scale.
}
\examples{
data(diamonds)
corner(diamonds)
model1 <- lm(price ~ carat + cut*color, data=diamonds)
model2 <- lm(price ~ carat*color, data=diamonds)
coefplot(model1)
coefplot(model1, shorten=FALSE)
coefplot(model1, shorten=c("cut"))
coefplot(model1, shorten=c("cut"), intercept=FALSE)
coefplot(model1, factors="cut")
coefplot(model1, factors="cut", only=TRUE)
coefplot(model1, facet=TRUE)
coefplot(model2)
}
\author{
  Jared P. Lander www.jaredlander.com
}
\seealso{
  \code{\link{lm}} \code{\link{glm}} \code{\link{rxLinMod}}
  \code{\link{ggplot}}
}
\keyword{coefficient}
\keyword{coefficients}
\keyword{coefplot}
\keyword{dotplot}
\keyword{glm}
\keyword{linear}
\keyword{lm}
\keyword{model}
\keyword{rxLinMod}

