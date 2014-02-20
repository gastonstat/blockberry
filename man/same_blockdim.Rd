\name{same_blockdim}
\alias{same_blockdim}
\alias{same_dims}
\alias{same_nblocks}
\alias{same_parts}
\title{Test for same block dimension}
\usage{
  same_blockdim(x, y)
}
\arguments{
  \item{x}{a multiblock object}

  \item{y}{a multiblock object}
}
\description{
  \code{same_blockdim} tests if two multiblock objects have
  same block-dimension \cr \code{same_parts} tests if two
  multiblock objects have same partitions \cr
  \code{same_dims} tests if two multiblock objects have
  same dimensions \cr \code{same_nblocks} tests if two
  multiblock objects have same number of blocks \cr
}
\examples{
# blockvectors
bx = blockvector(1:10, parts=c(5,5), dims=2)
by = blockvector(1:10, parts=c(2,3,5), dims=3)

# blockmatrix
bm = blockmatrix(matrix(1:12, 3, 4), 3, c(2,2))

# test if same blockdimensions
same_blockdim(bx, bx) # TRUE
same_blockdim(bm, bm) # TRUE
same_blockdim(bx, bm) # FALSE

# test if same partitions
same_parts(bx, bx) # TRUE
same_parts(bm, bm) # TRUE
same_parts(bx, bm) # FALSE

# test if same dimensions
same_dims(bx, bx) # TRUE
same_dims(bm, bm) # TRUE
same_dims(bx, bm) # FALSE

# test if same number of blocks
same_nblocks(bx, bx) # TRUE
same_nblocks(bx, by) # FALSE
}
\seealso{
  \code{\link{blockdim}}
}
\keyword{multiblock}

