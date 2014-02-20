\name{\%uu\%}
\alias{\%uu\%}
\alias{usual_usual}
\title{usual-usual product}
\usage{
  x \%uu\% y

  usual_usual(x, y)
}
\arguments{
  \item{x}{a numeric multiblock}

  \item{y}{a numeric multiblock}
}
\description{
  usual-usual product for multiblocks
}
\details{
  This product requires objects to have compatible
  block-dimensions
}
\examples{
# block vector
v = 1:5
bv = blockvector(v, parts=c(2, 3))
bv \%uu\% bv

# matrix
a = matrix(1:20, 4, 5)
rownames(a) = paste("ind", 1:4, sep='')
colnames(a) = c("a1", "a2", "b1", "b2", "b3")

# block-matrix and block-matrix
A = blockmatrix(a, c(2, 2), c(2, 3))
A \%uu\% t(A)

# block-matrix and block-vector
A \%uu\% bv
}
\seealso{
  \code{\link{scalar_scalar}}
}

