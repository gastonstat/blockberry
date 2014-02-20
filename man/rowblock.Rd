\name{rowblock}
\alias{colblock}
\alias{rowblock}
\title{Extract Row / Column Blocks}
\usage{
  rowblock(x, k)
}
\arguments{
  \item{x}{a multiblock}

  \item{k}{integer to indicate row (or column)
  block-dimension}
}
\value{
  the specified row/column block
}
\description{
  return the specified block(s) from a block-matrix Use
  \code{rowblock} or \code{colblock} depending on whether
  you want to get a particular row block or a particular
  column block (output as block-matrix object)
}
\examples{
# create a block-matrix with 6 blocks
A = blockmatrix(matrix(1:63, 7, 9), c(3,4), c(2,3,4))

# extract first row block
rowblock(A, 1)

# extract third column block
colblock(A, 3)
}
\seealso{
  \code{\link{get_block}}, \code{\link{separate}}
}

