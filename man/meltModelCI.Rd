\name{meltModelCI}
\alias{meltModelCI}
\title{Melt the modelCI}
\usage{
  meltModelCI(modelCI,
    keepCols = c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coefficient", "Value"),
    id.vars = c("Coefficient"), variable.name = "Type",
    value.name = "Value",
    outerCols = c("LowOuter", "HighOuter"),
    innerCols = c("LowInner", "HighInner"))
}
\arguments{
  \item{modelCI}{A \code{\link{data.frame}} as built by
  \code{\link{buildModelCI}}}

  \item{keepCols}{The columns in modelCI that should be
  kept as there can be extras}

  \item{id.vars}{The columns to use as ID variables in
  \code{\link{melt}}}

  \item{variable.name}{Used in \code{\link{melt}} for
  naming the column that stores the melted variables}

  \item{value.name}{Used in \code{\link{melt}} for naming
  the column that stores the melted values}

  \item{innerCols}{The columns to be included in the
  \code{\link{data.frame}} of inner standard errors}

  \item{outerCols}{The columns to be included in the
  \code{\link{data.frame}} of outer standard errors}
}
\value{
  A list consisting of \item{modelMelt}{Melted modelCI with
  all values} \item{modelMeltOuter}{modelMelt with only
  values associated with the outer standard errors}
  \item{modelMeltInner}{modelMelt with only values
  associated with the inner standard errors}
}
\description{
  Melt a modelCI into a form suitable for plotting
}
\details{
  \code{\link{buildModelCI}} builds a data.frame for
  plotting.  This function melts it into plottable form and
  seperates the coefficient data from the SE data into
  seprate data.frames
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut, data=diamonds)
modeled <- coefplot:::buildModelCI(model1)
coefplot:::meltModelCI(modeled)
}
\author{
  Jared P. Lander www.jaredlander.com
}
\seealso{
  \code{\link{coefplot}} \code{\link{buildModelCI}}
}

