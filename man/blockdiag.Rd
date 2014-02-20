\name{blockdiag}
\alias{bdiag}
\alias{blockdiag}
\title{Blockdiagonal matrix}
\usage{
  blockdiag(x)
}
\arguments{
  \item{x}{a blockvector or a blockmatrix}
}
\value{
  a block diagonal matrix
}
\description{
  Diagonalize a blockvactor, row-blockmatrix or a
  column-blockmatrix
}
\examples{
# create a blockvector
d = blockvector(data=1:4, parts=c(1,1,1,1), dims=4)
# blockdiagonal
blockdiag(d)

# create a regular matrix
a = matrix(runif(20), 5, 4)

# block-matrix with one single block
A1 = blockmatrix(a, 5, 4)
# block-matrix with one column block
A2 = blockmatrix(a, c(2, 3), 4)
# block-matrix with one row block
A3 = blockmatrix(a, 5, c(2,2))
# block-matrix with 2 row-blocks and 2 col-blocks
A4 = blockmatrix(a, c(2, 3), c(2, 2))

# block-diagonalizations
blockdiag(A1)
blockdiag(A2)
blockdiag(A3)
blockdiag(A4)
}

