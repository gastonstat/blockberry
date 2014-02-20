\name{nblocks}
\alias{nblocks}
\title{Number of Blocks}
\usage{
  nblocks(x)
}
\arguments{
  \item{x}{a multiblock object}
}
\value{
  the number of blocks
}
\description{
  return the number of blocks of a multiblock object
}
\examples{
# create some block-matrices
A = blockmatrix(matrix(1:12, 3, 4), 3, c(2,2))
B = blockmatrix(matrix(1:20, 5, 4), c(3,2), c(2,2))

# number of blocks
nblocks(A)
nblocks(B)
}
\seealso{
  \code{\link{blockdim}}
}

