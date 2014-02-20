#' @title hadamard-hadamard product
#' 
#' @description 
#' Hadamard-hadamard product of a blockmatrix and a blockmatrix \cr
#' 
#' @param x a block-matrix
#' @param y a block-matrix
#' @usage x \%hh\% y
#' @seealso \code{\link{hadamard_scalar}}
#' @export
#' @rdname hadamard_hadamard
#' @examples
#' # blockmatrix
#' X = blockmatrix(matrix(1:25, 5, 5), rowparts = c(2, 3), colparts = c(2, 3))
#' Y = blockmatrix(matrix(rep(1:5, 5), 5, 5), 
#'         rowparts = c(2, 3), colparts = c(2, 3))
#' 
#' # hadamard-hadamard
#' X %hh% Y
"%hh%" <- function(x, y) {
  hadamard_hadamard(x, y)
}


#' @rdname hadamard_hadamard
#' @S3method hadamard_hadamard default
#' @S3method hadamard_hadamard blockmatrix
hadamard_hadamard <- function(x, y) {
  UseMethod("hadamard_hadamard", x)
}

hadamard_hadamard.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%hh%'")
  if (!is.bmatrix(x) | !is.bmatrix(y))
    non_multiblock_args("'%hh%'")
}

hadamard_hadamard.blockmatrix <- function(x, y) 
{
  # bmatrix by bmatrix
  if (is.bmatrix(y)) {
    return(had_had_prod_bmatrix_bmatrix(x, y))  
  } else {
    invalid_args("'%hh%'")
  }
}


# private function
had_had_prod_bmatrix_bmatrix <- function(x, y) 
{
  # check 'x' and 'y' have same number of blocks
  if (!same_blockdim(x, y))
    incompatible_args("'%hh%'")

  # output
  x * y
}
