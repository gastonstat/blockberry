\name{\%uh\%}
\alias{\%uh\%}
\alias{usual_hadamard}
\title{usual-hadamard product}
\usage{
  x \%uh\% y

  usual_hadamard(x, y)
}
\arguments{
  \item{x}{a blockmatrix}

  \item{y}{a blockmatrix}
}
\description{
  usual-hadamard product for multiblocks
}
\examples{
# blockmatrices
A = blockmatrix(matrix(1:16, 4, 4), rowparts = 4, colparts = c(2, 2))
B = blockmatrix(matrix(1:16, 8, 2), rowparts = c(4, 4), colparts = 2)

A \%uh\% B
}
\seealso{
  \code{\link{scalar_scalar}}
}

