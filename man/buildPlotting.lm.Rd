\name{buildPlotting.lm}
\alias{buildPlotting.lm}
\title{Coefplot plotting}
\usage{
  buildPlotting.lm(modelCI, title = "Coefficient Plot",
  xlab = "Value", ylab = "Coefficient", lwdInner = 1,
  lwdOuter = 0, color = "blue", cex = 0.8, textAngle = 0,
  numberAngle = 0, outerCI = 2, innerCI = 1, multi = FALSE,
  zeroColor = "grey", zeroLWD = 1, zeroType = 2, numeric =
  FALSE, fillColor = "grey", alpha = 1/2, horizontal =
  FALSE, facet = FALSE, scales = "free")
}
\arguments{
  \item{modelCI}{An object created by
  \code{\link{buildModelCI}}}

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

  \item{multi}{logical; If this is for
  \code{\link{multiplot}} then dodge the geoms}

  \item{lwdInner}{The thickness of the inner confidence
  interval}

  \item{lwdOuter}{The thickness of the outer confidence
  interval}

  \item{color}{The color of the points and lines}

  \item{cex}{The text size multiplier, currently not used}

  \item{textAngle}{The angle for the coefficient labels, 0
  is horizontal}

  \item{numberAngle}{The angle for the value labels, 0 is
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

  \item{numeric}{logical; If true and factors has exactly
  one value, then it is displayed in a horizontal graph
  with constinuous confidence bounds.}

  \item{fillColor}{The color of the confidence bounds for a
  numeric factor}

  \item{alpha}{The transparency level of the numeric
  factor's confidence bound}

  \item{horizontal}{logical; If the plot should be
  displayed horizontally}
}
\value{
  a ggplot graph object
}
\description{
  Build ggplot object for coefplot
}
\details{
  This function builds up the ggplot layer by layer for
  \code{\link{coefplot.lm}}
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
theCI <- coefplot:::buildModelCI(model1)
theCIMelt <- coefplot:::meltModelCI(theCI)
coefplot:::buildPlotting.lm(theCI, theCIMelt$modelMeltInner, theCIMelt$modelMeltInner)
}
\author{
  Jared P. Lander www.jaredlander.com
}
\seealso{
  \code{\link{coefplot.lm}} \code{\link{coefplot}}
  \code{\link{multiplot}}
}

