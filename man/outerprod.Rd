\name{\%*o\%}
\alias{\%*o\%}
\alias{outerprod}
\title{Block-vector Outer Product}
\usage{
  x \%*o\% y

  outerprod(x, y)
}
\arguments{
  \item{x}{a block-vector}

  \item{y}{a block-vector}
}
\description{
  Outer product for (block)vectors.
}
\details{
  This product requires compatible objects
}
\examples{
# vector outer product
v = 1:5
v \%*o\% v

# block-vector outer product
bv = blockvector(v, parts=c(3, 2))
bv \%*o\% bv
}

