\name{collidev}
\alias{collidev}
\title{collidev}
\usage{
  collidev(data, height = NULL, name, strategy,
    check.height = TRUE)
}
\arguments{
  \item{data}{}

  \item{height}{}

  \item{name}{}

  \item{strategy}{}

  \item{check.height}{}
}
\value{
  Not sure
}
\description{
  Vertical collision checking
}
\details{
  A hacky adaptation of ggplot's collide function to be
  used for vertical collisions.  No warranties on this
  working except that it looks good for now.

  The adaptation switch x's to y's and y's to x's and width
  to height
}
\examples{
# none here
}
\author{
  Jared P. Lander
}

