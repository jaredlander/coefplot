\name{getModelInfo.lm}
\alias{getModelInfo.lm}
\title{Model Information}
\usage{
  getModelInfo.lm(model, shorten = TRUE, factors = NULL,
    only = NULL, ...)
}
\arguments{
  \item{model}{The fitted model with coefficients to be
  plotted}

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

  \item{\dots}{Further arguments}
}
\value{
  Information on the model
}
\description{
  Extracts and builds extensive information from lm and glm
  models
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

