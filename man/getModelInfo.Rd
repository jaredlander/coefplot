\name{getModelInfo}
\alias{getModelInfo}
\title{Model Information}
\usage{
  getModelInfo(model, ...)
}
\arguments{
  \item{model}{The fitted model with coefficients to be
  plotted}

  \item{\dots}{Further arguments such as shorten, only and
  factors}
}
\value{
  Information on the model
}
\description{
  Extracts and builds extensive information from models
}
\details{
  Helper function for \code{\link{coefplot}}
}
\examples{
data(diamonds)
model1 <- lm(price ~ carat + cut*color, data=diamonds)
coefplot(model1)
}
\author{
  Jared P. Lander
}
\seealso{
  \code{\link{coefplot.lm}}
}

