\name{\%su\%}
\alias{\%su\%}
\alias{scalar_usual}
\title{scalar-usual product}
\usage{
  x \%su\% y

  scalar_usual(x, y)
}
\arguments{
  \item{x}{a matrix}

  \item{y}{a blockmatrix}
}
\description{
  scalar-usual product between a matrix and a blockmatrix
}
\examples{
# blockmatrix
X = blockmatrix(matrix(1:12, 3, 4), rowparts = c(3), colparts = c(2, 2))

# matrix
m = matrix(1:6, 2, 3)

# scalar-usual product
m \%su\% X
}
\seealso{
  \code{\link{scalar_scalar}}
}

