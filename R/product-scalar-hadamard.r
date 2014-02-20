#' @title scalar-hadamard product
#' 
#' @description 
#' Simple hadamard of a matrix and a blockmatrix \cr
#' 
#' @param x a matrix
#' @param y a block-matrix
#' @usage x \%sh\% y
#' @seealso \code{\link{scalar_hadamard}}
#' @export
#' @rdname scalar_hadamard
#' @examples
#' # blockmatrix
#' X = blockmatrix(matrix(1:48, 6, 8), rowparts = c(3, 3), colparts = c(4, 4))
#' 
#' # matrix
#' m = matrix(rep(c(1,2), 6), 3, 4, byrow = TRUE)
#' 
#' # scalar-hadamard
#' m %sh% X
"%sh%" <- function(x, y) {
  scalar_hadamard(x, y)
}


#' @rdname scalar_hadamard
#' @S3method scalar_hadamard default
#' @S3method scalar_hadamard matrix
scalar_hadamard <- function(x, y) {
  UseMethod("scalar_hadamard", x)
}

scalar_hadamard.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%sh%'")
  if (is_not_matrix(x))
    invalid_args("'%sh%'")
  if (!is.bmatrix(y))
    non_multiblock_args("'%sh%'")
}

scalar_hadamard.numeric <- function(x, y) {
  x * y
}

scalar_hadamard.matrix <- function(x, y) 
{
  # matrix by bmatrix
  if (is.bmatrix(y)) {
    return(scal_had_prod_matrix_bmatrix(x, y))  
  } else {
    invalid_args("'%sh%'")
  }
}


# private function
scal_had_prod_matrix_bmatrix <- function(x = NULL, y = NULL) 
{
  # check all rowparts(y) are equal
  if (length(unique(rowparts(y))) > 1)
    stop("\nrowparts of blockmatrix are not equal")
  # check all colparts(y) are equal
  if (length(unique(colparts(y))) > 1)
    stop("\ncolparts of blockmatrix are not equal")
  
  # compatibility between 'x' and 'y'
  if (nrow(x) != rowparts(y)[1L])
    incompatible_args("'%sh%'")
  if (ncol(x) != colparts(y)[1L])
    incompatible_args("'%sh%'")
  
  # product
  result = blockmatrix(matrix(0, nrow(y), ncol(y)), 
                       rowparts(y), colparts(y))
  # from and to indices
  rows = from_to(rowparts(result))
  cols = from_to(colparts(result))
  
  for (i in 1:brow(y)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    for (j in 1:bcol(y)) 
    {
      # specified starting and ending indices
      jcols = cols$from[j]:cols$to[j]
      # extract block 'ij'
      Y_ij = get_block(y, i, j)
      result[irows,jcols] = x * Y_ij
    }
  }
  
  # output
  result 
}
