\name{multiplot}
\alias{multiplot}
\title{Plot multiple coefplots}
\usage{
  multiplot(..., title = "Coefficient Plot", xlab =
  "Value", ylab = "Coefficient", innerCI = 1, outerCI = 2,
  lwdInner = 1, lwdOuter = 0, color = "blue", cex = 0.8,
  textAngle = 0, numberAngle = 90, zeroColor = "grey",
  zeroLWD = 1, zeroType = 2, single = TRUE, scales =
  "fixed", ncol = length(unique(modelCI$Model)), sort =
  "natural", decreasing = FALSE, names = NULL, numeric =
  FALSE, fillColor = "grey", alpha = 1/2, horizontal =
  FALSE, factors = NULL, only = NULL, shorten = TRUE,
  intercept = TRUE, plot = TRUE, drop = FALSE)
}
\arguments{
  \item{\dots}{Models to be plotted}

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

  \item{single}{logical; If TRUE there will be one plot
  with the points and bars stacked, otherwise the models
  will be displayed in seperate facets}

  \item{scales}{The way the axes should be treated in a
  faceted plot.  Can be c("fixed", "free", "free_x",
  "free_y")}

  \item{ncol}{The number of columns that the models should
  be plotted in}

  \item{sort}{Determines the sort order of the
  coefficients.  Possible values are c("natural", "normal",
  "magnitude", "size", "alphabetical")}

  \item{decreasing}{logical; Whether the coefficients
  should be ascending or descending}

  \item{names}{Names for models, if NULL then they will be
  numerically labelled}

  \item{numeric}{logical; If true and factors has exactly
  one value, then it is displayed in a horizontal graph
  with constinuous confidence bounds.}

  \item{fillColor}{The color of the confidence bounds for a
  numeric factor}

  \item{alpha}{The transparency level of the numeric
  factor's confidence bound}

  \item{horizontal}{logical; If the plot should be
  displayed horizontally}

  \item{intercept}{logical; Whether the Intercept
  coefficient should be plotted}

  \item{plot}{logical; If the plot should be drawn, if
  false then a data.frame of the values will be returned}

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

  \item{drop}{logical; if TRUE then models without valid
  coeffiecients to show will not be plotted}
}
\value{
  A ggplot object
}
\description{
  Plot the coeffcients from multiple models
}
\details{
  Plots a graph similar to \code{\link{coefplot}} but for
  multiple plots at once.

  For now, if \code{names} is provided the plots will
  appear in alphabetical order of the names.  This wil be
  adjusted in future iterations.
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
model2 <- lm(price ~ carat + cut + color, data=diamonds)
model3 <- lm(price ~ carat + color, data=diamonds)
multiplot(model1, model2, model3)
multiplot(model1, model2, model3, single=FALSE)
multiplot(model1, model2, model3, factors="color")
multiplot(model1, model2, model3, factors="color", drop=TRUE)
multiplot(model1, model2, model3, plot=FALSE)
}
\seealso{
  \code{link{coefplot}}
}

