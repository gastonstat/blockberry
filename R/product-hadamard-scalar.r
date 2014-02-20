#' @title hadamard-scalar product
#' 
#' @description 
#' Hadamard-scalar product of a matrix and a blockmatrix \cr
#' 
#' @param x a matrix
#' @param y a block-matrix
#' @usage x \%hs\% y
#' @seealso \code{\link{hadamard_usual}}
#' @export
#' @rdname hadamard_scalar
#' @examples
#' # blockmatrix
#' X = blockmatrix(matrix(1:12, 3, 4), rowparts = 3, colparts = c(2, 2))
#' 
#' # matrix
#' m = matrix(rep(c(1,2), 6), 3, 4, byrow = TRUE)
#' 
#' # hadamard-scalar
#' m %hs% X
"%hs%" <- function(x, y) {
  hadamard_scalar(x, y)
}


#' @rdname hadamard_scalar
#' @S3method hadamard_scalar default
#' @S3method hadamard_scalar matrix
hadamard_scalar <- function(x, y) {
  UseMethod("hadamard_scalar", x)
}

hadamard_scalar.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%hs%'")
  if (is_not_matrix(x))
    invalid_args("'%hs%'")
  if (!is.bmatrix(y))
    non_multiblock_args("'%hs%'")
}

hadamard_scalar.numeric <- function(x, y) {
  x * y
}

hadamard_scalar.matrix <- function(x, y) 
{
  # matrix by bmatrix
  if (is.bmatrix(y)) {
    return(hadscalprod_matrix_bmatrix(x, y))  
  } else {
    invalid_args("'%hs%'")
  }
}


# private function
hadscalprod_matrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!identical(dim(x), dim(y)))
    incompatible_args("'%hs%'")
  
  # output
  x * y
}


