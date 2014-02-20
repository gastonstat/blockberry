\name{bvnorm}
\alias{bvnorm}
\title{Blockvector Norm}
\usage{
  bvnorm(x)
}
\arguments{
  \item{x}{a blockvector}
}
\value{
  norm of x
}
\description{
  Computes the norm(s) of a blockvector
}
\examples{
# numeric blockvector
bv = blockvector(1:10, c(4,6), 2)
bvnorm(bv)
}

