\name{multiplot}
\alias{multiplot}
\title{Plot multiple coefplots}
\usage{
  multiplot(...)
}
\arguments{
  \item{\dots}{Models to be plotted}
}
\value{
  A ggplot object
}
\description{
  Plot the coeffcients from multiple models
}
\details{
  Plots a graph similar to \code{\link{coefplot}} but for
  multiple plots at once
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
model2 <- lm(price ~ carat + cut + color, data=diamonds)
multiplot(model1, model2)
}
\seealso{
  \code{link{coefplot}}
}

