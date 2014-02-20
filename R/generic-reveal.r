#' @title reveal blockdimension structure
#' 
#' @description
#' Reveals the blockdimension structure in a visual display \cr
#' 
#' @param x a multiblock object
#' @seealso \code{\link{nblocks}}, \code{\link{blockdim}}
#' @export
#' @S3method reveal default
#' @S3method reveal blockvector
#' @S3method reveal blockmatrix
#' @examples
#' # create a block-vector
#' bnum = blockvector(runif(10), parts = c(5,5), dims = 2)
#' reveal(bnum)
#' 
#' # another blockvector
#' g = blockvector(1:100, parts = rep(10, 10), dims = 10)
#' reveal(g)
#' 
#' # create a block-matrix
#' a = matrix(runif(20), 5, 4)
#' A = blockmatrix(a, c(2, 3), c(2, 2))
#' reveal(A)
#' 
#' # another block-matrix
#' b = matrix(runif(100), 5, 20)
#' B = blockmatrix(b, c(2, 3), c(10, 10))
#' reveal(B)
reveal <- function(x) {
  UseMethod("reveal", x)
}

reveal.default <- function(x) {
  if (!is.multiblock(x))
    stop("\nreveal() requires a multiblock")
}

reveal.blockvector <- function(x) 
{
  v = rep("+", nblocks(x))
  names(v) = parts(x)
  # output
  cat("blockvector", "\n\n")
  print(v, quote = FALSE)
}

reveal.blockmatrix <- function(x)
{
  m = matrix("+", bdim(x)[1L], bdim(x)[2L])
  rownames(m) = rowparts(x)
  colnames(m) = colparts(x)
  # output
  cat("blockmatrix", "\n\n")
  print(m, quote = FALSE)
}


#reveal.blocktensor <- function(x) {
#  # output
#  cat("blocktensor", "\n\n")
#  print(m, quote = FALSE)
#}
