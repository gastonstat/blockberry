\name{bdim}
\alias{bcol}
\alias{bdim}
\alias{brow}
\title{Number of Blocks by Rows/Columns of a Block-Matrix}
\usage{
  bdim(x)
}
\arguments{
  \item{x}{a block-matrix (object of class
  \code{"blockmatrix"})}
}
\value{
  an integer indicating the number of blocks
}
\description{
  return the number of rows present in a block-matrix
}
\details{
  Use \code{bdim} to get the blockdimension \cr Use
  \code{brow} to get the number of blocks by rows \cr Use
  \code{bcol} to get the number of blocks by columns
}
\examples{
# create some block-matrices
A = blockmatrix(matrix(1:12, 3, 4), rowparts=3, colparts=c(2,2))
B = blockmatrix(matrix(1:20, 5, 4), rowparts=c(3,2), colparts=c(2,2))

# row-dimensions
brow(A)
brow(B)

# column-dimensions
bcol(A)
bcol(B)
}
\seealso{
  \code{\link{blockdim}}
}

