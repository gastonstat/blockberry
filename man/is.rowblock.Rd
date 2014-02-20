\name{is.rowblock}
\alias{is.colblock}
\alias{is.megablock}
\alias{is.oneblock}
\alias{is.rowblock}
\title{Test block-matrix structure}
\usage{
  is.rowblock(x)
}
\arguments{
  \item{x}{an R object}
}
\description{
  \code{is.rowblock} tests if its argument is one row
  block-matrix \cr \code{is.colblock} tests if its argument
  is one column block-matrix \cr \code{is.oneblock} tests
  if its argument is one single block-matrix \cr
  \code{is.megablock} tests if its argument is a
  block-matrix with more than one block by rows and columns
}
\examples{
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

# test if row-block
is.rowblock(A1)  # TRUE
is.rowblock(A2)  # FALSE
is.rowblock(A3)  # TRUE
is.rowblock(A4)  # FALSE

# test if col-block
is.colblock(A1)  # TRUE
is.colblock(A2)  # TRUE
is.colblock(A3)  # FALSE
is.colblock(A4)  # FALSE

# test if one-block
is.oneblock(A1)  # TRUE
is.oneblock(A2)  # FALSE
is.oneblock(A3)  # FALSE
is.oneblock(A4)  # FALSE

# test if mega-block
is.megablock(A1)  # FALSE
is.megablock(A2)  # FALSE
is.megablock(A3)  # FALSE
is.megablock(A4)  # TRUE
}

