% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coefpath.r
\name{coefpath}
\alias{coefpath}
\alias{coefpath.glmnet}
\alias{coefpath.cv.glmnet}
\title{coefpath}
\usage{
coefpath(model, ...)

\method{coefpath}{glmnet}(
  model,
  xlab = "Log Lambda",
  ylab = "Coefficients",
  showLegend = c("onmouseover", "auto", "always", "follow", "never"),
  annotate = TRUE,
  elementID = NULL,
  ...
)

\method{coefpath}{cv.glmnet}(
  model,
  xlab = "Log Lambda",
  ylab = "Coefficients",
  showLegend = c("onmouseover", "auto", "always", "follow", "never"),
  annotate = TRUE,
  colorMin = "black",
  strokePatternMin = "dotted",
  labelMin = "lambda.min",
  locMin = c("bottom", "top"),
  color1se = "black",
  strokePattern1se = "dotted",
  label1se = "lambda.1se",
  loc1se = c("bottom", "top"),
  ...
)
}
\arguments{
\item{model}{A \code{\link[glmnet]{glmnet}} model}

\item{\dots}{Arguments passed on to \code{\link{extractPath}}}

\item{xlab}{x-axis label}

\item{ylab}{y-axis label}

\item{showLegend}{When to display the legend. Specify "always" to always show the legend. Specify "onmouseover" to only display it when a user mouses over the chart. Specify "follow" to have the legend show as overlay to the chart which follows the mouse. The default behavior is "auto", which results in "always" when more than one series is plotted and "onmouseover" when only a single series is plotted.}

\item{annotate}{If \code{TRUE} (default) plot the name of the series}

\item{elementID}{Unique identified for dygraph, if \code{NULL} it will be randomly generated}

\item{colorMin}{Color for line showing lambda.min}

\item{strokePatternMin}{Stroke pattern for line showing lambda.min}

\item{labelMin}{Label for line showing lambda.min}

\item{locMin}{Location for line showing lambda.min, can be 'bottom' or 'top'}

\item{color1se}{Color for line showing lambda.1se}

\item{strokePattern1se}{Stroke pattern for line showing lambda.1se}

\item{label1se}{Label for line showing lambda.1se}

\item{loc1se}{Location for line showing lambda.1se, can be 'bottom' or 'top'}
}
\value{
A dygraphs object
}
\description{
Visualize the coefficient path resulting from the elastic net
}
\details{
This is a replacement plot for visualizing the coefficient path resulting from the elastic net. This allows for interactively inspecting the plot so it is easier to disambiguate the coefficients.
}
\examples{

\dontshow{if(requireNamespace('glmnet', quietly=TRUE))\{}
library(glmnet)
library(ggplot2)
library(useful)
data(diamonds)
diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts = TRUE)
diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)
modG1 <- glmnet(x=diaX, y=diaY)
coefpath(modG1)

modG2 <- cv.glmnet(x=diaX, y=diaY, nfolds=5)
coefpath(modG2)

x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100)
fit1 <- glmnet(x, y)
coefpath(fit1)
\dontshow{\}}

}
\author{
Jared P. Lander
}
