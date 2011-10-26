\name{coefplot}
\alias{coefplot}
\alias{plotcoef}
\title{Dotplot for model coefficients}
\usage{
  coefplot(model, ...)
}
\arguments{
  \item{model}{The fitted model with coefficients to be
  plotted}

  \item{\dots}{See \code{\link{coefplot.lm}} for argument
  details}
}
\value{
  A ggplot2 object or data.frame.  See details in
  \code{\link{coefplot.lm}} for more information
  www.jaredlander.com
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

  A future iteration of \code{coefplot.glm} will also allow
  for plotting the coefficients on the transformed scale.

  See \code{\link{coefplot.lm}} for specific documentation
  and the return value.
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
  Jared P. Lander
}
\seealso{
  \code{\link{coefplot.lm}}
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

