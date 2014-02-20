\name{\%ss\%}
\alias{\%ss\%}
\alias{scalar_scalar}
\title{scalar-scalar product}
\usage{
  x \%ss\% y

  scalar_scalar(x, y)
}
\arguments{
  \item{x}{either a scalar or a matrix}

  \item{y}{a multiblock}
}
\description{
  Simple scalar by a blockmatrix \cr Simple scalar by a
  blockvector \cr
}
\examples{
# block vector
bv = blockvector(1:5, parts = c(2, 3))
5 \%ss\% bv

# same as
5 * bv

# blockmatrix
X = blockmatrix(matrix(1:12, 3, 4), rowparts = c(3), colparts = c(2, 2))
5 \%ss\% X

# matrix
m = matrix(1:6, 3, 2)
m \%ss\% X
}
\seealso{
  \code{\link{scalar_usual}}
}

