#' @title scalar-usual product
#' 
#' @description 
#' scalar-usual product between a matrix and a blockmatrix
#' 
#' @param x a matrix
#' @param y a blockmatrix
#' @usage x \%su\% y
#' @seealso \code{\link{scalar_scalar}}
#' @export
#' @rdname scalar_usual
#' @examples
#' # blockmatrix
#' X = blockmatrix(matrix(1:12, 3, 4), rowparts = c(3), colparts = c(2, 2))
#' 
#' # matrix
#' m = matrix(1:6, 2, 3)
#' 
#' # scalar-usual product
#' m %su% X
"%su%" <- function(x, y) {
  scalar_usual(x, y)
}


#' @rdname scalar_usual
#' @S3method scalar_usual default
#' @S3method scalar_usual matrix
scalar_usual <- function(x, y) {
  UseMethod("scalar_usual", x)
}

scalar_usual.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%su%'")
  if (is_not_matrix(x))
    invalid_args("'%su%'")
  if (!is.multiblock(y))
    non_multiblock_args("'%su%'")
}

scalar_usual.matrix <- function(x, y) 
{
  # matrix by bmatrix
  if (is.bmatrix(y)) {
    return(scalar_usual_prod_matrix_bmatrix(x, y))  
  } else {
    invalid_args("'%su%'")
  }
}


# private function
scalar_usual_prod_matrix_bmatrix <- function(x = NULL, y = NULL) 
{
  # extract row and column dimensions of each block in 'y'
  start_end = from_to(dims(y))
  rows = parts(y)[start_end$from[1L]:start_end$to[1L]]
  
  # check ncol(y) == all nrow of blocks
  if (any(ncol(x) != rows))
    incompatible_args("'%su%'")
  
  # product
  result = blockmatrix(matrix(0, nrow(x)*brow(y), ncol(y)), 
                       rep(nrow(x),brow(y)), colparts(y))
  # from and to indices
  rows = from_to(rowparts(result))
  cols = from_to(colparts(result))
  
  for (i in 1:brow(y)) 
  {
    irows = rows$from[i]:rows$to[i]
    for (j in 1:bcol(y)) 
    {
      # extract block 'ij'
      Y_ij = get_block(y, i=i, j=j)
      # specified starting and ending indices
      jcols = cols$from[j]:cols$to[j]
      # usual product
      result[irows,jcols] = x %*% Y_ij
    }
  }
  
  # output
  result
}
