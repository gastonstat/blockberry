\name{\%hs\%}
\alias{\%hs\%}
\alias{hadamard_scalar}
\title{hadamard-scalar product}
\usage{
  x \%hs\% y

  hadamard_scalar(x, y)
}
\arguments{
  \item{x}{a matrix}

  \item{y}{a block-matrix}
}
\description{
  Hadamard-scalar product of a matrix and a blockmatrix \cr
}
\examples{
# blockmatrix
X = blockmatrix(matrix(1:12, 3, 4), rowparts = 3, colparts = c(2, 2))

# matrix
m = matrix(rep(c(1,2), 6), 3, 4, byrow = TRUE)

# hadamard-scalar
m \%hs\% X
}
\seealso{
  \code{\link{hadamard_usual}}
}

