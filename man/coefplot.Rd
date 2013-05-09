\docType{package}
\name{coefplot}
\alias{coefplot}
\alias{coefplot-package}
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
}
\description{
  A graphical display of the coefficients and standard
  errors from a fitted model

  Provides an S3 generic method for plotting coefficients
  from a model so it can be extended to other model types.
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

  Currently, methods are available for lm, glm and rxLinMod
  objects.
}
\examples{
data(diamonds)
head(diamonds)
model1 <- lm(price ~ carat + cut*color, data=diamonds)
model2 <- lm(price ~ carat*color, data=diamonds)
model3 <- glm(price > 10000 ~ carat*color, data=diamonds)
coefplot(model1)
coefplot(model2)
coefplot(model3)
coefplot(model1, predictors="color")
coefplot(model1, predictors="color", strict=TRUE)
coefplot(model1, coefficients=c("(Intercept)", "color.Q"))
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"), strict=TRUE)
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"), strict=FALSE)
coefplot(model1, predictors="cut", coefficients=c("(Intercept)", "color.Q"),
strict=TRUE, newNames=c(color.Q="Color", "cut^4"="Fourth"))
coefplot(model1, predictors=c("(Intercept)", "carat"), newNames=c(carat="Size"))
coefplot(model1, predictors=c("(Intercept)", "carat"),
newNames=c(carat="Size", "(Intercept)"="Constant"))
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

