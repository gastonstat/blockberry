\name{blockdim}
\alias{"blockdim<-"}
\alias{blockdim}
\alias{blockdim<-}
\alias{dims}
\alias{ord}
\alias{parts}
\title{block-dimension of a multi-block object}
\usage{
  blockdim(x)

  blockdim(x) <- value
}
\arguments{
  \item{x}{a multiblock object}

  \item{value}{a list with new \code{parts} and
  \code{dims}}
}
\description{
  \code{blockdim} returns the blockdimension \cr
  \code{parts} returns the partitions \cr \code{dims}
  returns the dimensions \cr \code{ord} returns the order
  \cr
}
\examples{
# blockvetcor
bv = blockvector(1:10, parts=c(5,5), dims=2)
blockdim(bv)

# blockdimension associated to a blockmatrix
M = matrix(1:70, 10, 7)
bm = blockmatrix(M, parts=c(5,5,3,4), dims=c(2,2))
blockdim(bm)
parts(bm)
dims(bm)
ord(bm)

# old blockvector
old_one = blockvector(1:10, 10, 1)
blockdim(old_one)

# new blockvector
new_one = old_one

# replace blockdimension in new blockvector
blockdim(new_one) = list(parts=c(3,7), dims=2)
blockdim(new_one)
}
\seealso{
  \code{\link{nblocks}}, \code{\link{same_blockdim}}
}

