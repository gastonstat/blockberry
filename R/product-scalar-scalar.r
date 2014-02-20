#' @title scalar-scalar product
#' 
#' @description 
#' Simple scalar by a blockmatrix \cr
#' Simple scalar by a blockvector \cr
#' 
#' @param x either a scalar or a matrix
#' @param y a multiblock
#' @usage x \%ss\% y
#' @seealso \code{\link{scalar_usual}}
#' @export
#' @rdname scalar_scalar
#' @examples
#' # block vector
#' bv = blockvector(1:5, parts = c(2, 3))
#' 5 %ss% bv
#' 
#' # same as
#' 5 * bv
#' 
#' # blockmatrix
#' X = blockmatrix(matrix(1:12, 3, 4), rowparts = c(3), colparts = c(2, 2))
#' 5 %ss% X
#' 
#' # matrix
#' m = matrix(1:6, 3, 2)
#' m %ss% X
"%ss%" <- function(x, y) {
  scalar_scalar(x, y)
}


#' @rdname scalar_scalar
#' @S3method scalar_scalar default
#' @S3method scalar_scalar numeric
#' @S3method scalar_scalar matrix
scalar_scalar <- function(x, y) {
  UseMethod("scalar_scalar", x)
}

scalar_scalar.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%ss%'")
  if (is_not_scalar(x) | is_not_matrix(x))
    invalid_args("'%ss%'")
  if (!is.multiblock(y))
    non_multiblock_args("'%ss%'")
}

scalar_scalar.numeric <- function(x, y) {
  x * y
}

scalar_scalar.matrix <- function(x, y) 
{
  # matrix by bmatrix
  if (is.bmatrix(y)) {
    return(scal_scal_prod_matrix_bmatrix(x, y))  
  } else {
    invalid_args("'%ss%'")
  }
}


# private function
scal_scal_prod_matrix_bmatrix <- function(x = NULL, y = NULL) 
{
  # extract row and column dimensions of each block in 'y'
  start_end = from_to(dims(y))
  rows = parts(y)[start_end$from[1L]:start_end$to[1L]]
  cols = parts(y)[start_end$from[2L]:start_end$to[2L]]

  # check compatible row dimensions
  if (any(nrow(x) != rows))
    incompatible_args("'%ss%'")
  # check compatible column dimensions
  if (any(ncol(x) != cols))
    incompatible_args("'%ss%'")
  
  # product
  result = blockmatrix(matrix(0, nrow(y), ncol(y)), 
                       rowparts(y), colparts(y))
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
      result[irows,jcols] = x * Y_ij
    }
  }
  
  # output
  result
}
