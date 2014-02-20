\name{\%sh\%}
\alias{\%sh\%}
\alias{scalar_hadamard}
\title{scalar-hadamard product}
\usage{
  x \%sh\% y

  scalar_hadamard(x, y)
}
\arguments{
  \item{x}{a matrix}

  \item{y}{a block-matrix}
}
\description{
  Simple hadamard of a matrix and a blockmatrix \cr
}
\examples{
# blockmatrix
X = blockmatrix(matrix(1:48, 6, 8), rowparts = c(3, 3), colparts = c(4, 4))

# matrix
m = matrix(rep(c(1,2), 6), 3, 4, byrow = TRUE)

# scalar-hadamard
m \%sh\% X
}
\seealso{
  \code{\link{scalar_hadamard}}
}

