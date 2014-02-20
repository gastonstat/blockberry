\name{reveal}
\alias{reveal}
\title{reveal blockdimension structure}
\usage{
  reveal(x)
}
\arguments{
  \item{x}{a multiblock object}
}
\description{
  Reveals the blockdimension structure in a visual display
  \cr
}
\examples{
# create a block-vector
bnum = blockvector(runif(10), parts = c(5,5), dims = 2)
reveal(bnum)

# another blockvector
g = blockvector(1:100, parts = rep(10, 10), dims = 10)
reveal(g)

# create a block-matrix
a = matrix(runif(20), 5, 4)
A = blockmatrix(a, c(2, 3), c(2, 2))
reveal(A)

# another block-matrix
b = matrix(runif(100), 5, 20)
B = blockmatrix(b, c(2, 3), c(10, 10))
reveal(B)
}
\seealso{
  \code{\link{nblocks}}, \code{\link{blockdim}}
}

