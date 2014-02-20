\name{\%hu\%}
\alias{\%hu\%}
\alias{hadamard_usual}
\title{hadamard-usual product}
\usage{
  x \%hu\% y

  hadamard_usual(x, y)
}
\arguments{
  \item{x}{a block-matrix}

  \item{y}{a block-matrix}
}
\description{
  Hadamard-usual product of a blockmatrix and a blockmatrix
  \cr
}
\examples{
# blockmatrix
X = blockmatrix(matrix(1:25, 5, 5), rowparts = c(2, 3), colparts = c(2, 3))
Y = blockmatrix(matrix(rep(1:5, 5), 5, 5),
        rowparts = c(2, 3), colparts = c(2, 3))

# hadamard-usual
# X \%hu\% Y
}
\seealso{
  \code{\link{hadamard_scalar}}
}

