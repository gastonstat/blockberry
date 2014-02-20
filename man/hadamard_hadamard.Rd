\name{\%hh\%}
\alias{\%hh\%}
\alias{hadamard_hadamard}
\title{hadamard-hadamard product}
\usage{
  x \%hh\% y

  hadamard_hadamard(x, y)
}
\arguments{
  \item{x}{a block-matrix}

  \item{y}{a block-matrix}
}
\description{
  Hadamard-hadamard product of a blockmatrix and a
  blockmatrix \cr
}
\examples{
# blockmatrix
X = blockmatrix(matrix(1:25, 5, 5), rowparts = c(2, 3), colparts = c(2, 3))
Y = blockmatrix(matrix(rep(1:5, 5), 5, 5),
        rowparts = c(2, 3), colparts = c(2, 3))

# hadamard-hadamard
X \%hh\% Y
}
\seealso{
  \code{\link{hadamard_scalar}}
}

