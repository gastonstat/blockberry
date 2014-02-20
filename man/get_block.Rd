\name{get_block}
\alias{get_block}
\title{Extract Blocks}
\usage{
  get_block(x, i, j, k)
}
\arguments{
  \item{x}{a multiblock}

  \item{i}{integer to indicate row block-dimension}

  \item{j}{integer to indicate column block-dimension}

  \item{k}{integer to indicate slice block-dimension (not
  implemented yet)}
}
\value{
  the specified block
}
\description{
  return the specified block(s) from a multi-block
}
\details{
  Use \code{get_block} to extract one single block from a
  multi-block object
}
\examples{
# blockvetcor
bv = blockvector(1:10, parts=c(5,5), dims=2)
# first block
get_block(bv, 1)
# second block
get_block(bv, 2)

# create a block-matrix with 6 blocks
A = blockmatrix(matrix(1:63, 7, 9), c(3,4), c(2,3,4))

# extract block A11
get_block(A, 1, 1)

# extract block A23
get_block(A, 2, 3)
}
\seealso{
  \code{\link{rowblock}}, \code{\link{colblock}},
  \code{\link{separate}}
}

