\name{blockmatrix}
\alias{as.blockmatrix}
\alias{as.bmatrix}
\alias{blockmatrix}
\alias{is.blockmatrix}
\alias{is.bmatrix}
\title{Block Matrix}
\usage{
  blockmatrix(data, rowparts = NULL, colparts = NULL,
    parts = NULL, dims = NULL)

  is.blockmatrix(x)

  as.blockmatrix(x, ...)
}
\arguments{
  \item{data}{a matrix, a vector, or a blockvector}

  \item{rowparts}{vector of row partitions}

  \item{colparts}{vector of column partitions}

  \item{parts}{optional integer positive vector with the
  partitions}

  \item{dims}{optional integer positive vector with the
  block structure}

  \item{x}{an R object}

  \item{...}{additional arguments to be passed to
  \code{blockmatrix}}
}
\value{
  An object of class \code{"blockmatrix"}
}
\description{
  \code{blockmatrix} creates a block-matrix from the given
  set of values \cr \code{as.bmatrix} attempts to turn its
  argument into a block-matrix \cr \code{is.bmatrix} tests
  if its argument is a (strict) block-matrix
}
\details{
  When \code{data} is a blockvector, the return blockmatrix
  is a column-block (i.e. vertical) matrix
}
\examples{
# create a regular matrix
a = matrix(runif(20), 5, 4)

# convert 'a' into a block-matrix
A = blockmatrix(a, c(2, 3), c(2, 2))
A

# test if block-matrix
is.bmatrix(a)  # FALSE
is.bmatrix(A)  # TRUE

# generate a matrix
m = matrix(1:9, 3, 3)

# convert 'm' into a block-matrix (one block)
M = as.bmatrix(m, 3, 3)
M
}
\seealso{
  \code{\link{blockvector}}, \code{\link{matrix}}
}

