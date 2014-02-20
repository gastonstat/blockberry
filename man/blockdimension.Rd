\name{blockdimension}
\alias{blockdimension}
\title{Create a blockdimension object}
\usage{
  blockdimension(parts, dims)
}
\arguments{
  \item{parts}{an integer positive vector with the
  partitions}

  \item{dims}{an integer positive vector with the block
  structure}
}
\value{
  a blockdim object
}
\description{
  blockdimension creates a blockdimension object with the
  specified values.
}
\examples{
# blockdimension associated to a blockvector
blockdimension(parts=c(5,5), dims=2)

# blockdimension associated to a blockmatrix
blockdimension(parts=c(5,5,3,4), dims=c(2,2))
}
\seealso{
  \code{\link{parts}}
}
\keyword{classes}
\keyword{multiblock}

