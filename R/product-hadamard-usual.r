#' @title hadamard-usual product
#' 
#' @description 
#' Hadamard-usual product of a blockmatrix and a blockmatrix \cr
#' 
#' @param x a block-matrix
#' @param y a block-matrix
#' @usage x \%hu\% y
#' @seealso \code{\link{hadamard_scalar}}
#' @export
#' @rdname hadamard_usual
#' @examples
#' # blockmatrix
#' X = blockmatrix(matrix(1:25, 5, 5), rowparts = c(2, 3), colparts = c(2, 3))
#' Y = blockmatrix(matrix(rep(1:5, 5), 5, 5), 
#'         rowparts = c(2, 3), colparts = c(2, 3))
#' 
#' # hadamard-usual
#' # X %hu% Y
"%hu%" <- function(x, y) {
  hadamard_usual(x, y)
}


#' @rdname hadamard_usual
#' @S3method hadamard_usual default
#' @S3method hadamard_usual blockmatrix
hadamard_usual <- function(x, y) {
  UseMethod("hadamard_usual", x)
}

hadamard_usual.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%hu%'")
  if (!is.bmatrix(x) | !is.bmatrix(y))
    non_multiblock_args("'%hu%'")
}

hadamard_usual.blockmatrix <- function(x, y) 
{
  # bmatrix by bmatrix
  if (is.bmatrix(y)) {
    return(had_usual_prod_bmatrix_bmatrix(x, y))  
  } else {
    invalid_args("'%hu%'")
  }
}


# private function
had_usual_prod_bmatrix_bmatrix <- function(x, y) 
{
  # check 'x' and 'y' have same number of blocks
  if (!same_nblocks(x, y))
    incompatible_args("'%hu%'")
  # check same number of block-rows
  if (brow(x) != brow(y))
    incompatible_args("'%hu%'")
  # check same number of block-columns
  if (bcol(x) != bcol(y))
    incompatible_args("'%hu%'")
  
  # check row and column dimensions compatibility
  if (length(unique(colparts(x))) > 1)
    stop("\nall colparts of 'x' must be identical")
  if (length(unique(rowparts(y))) > 1)
    stop("\nall rowparts of 'y' must be identical")
  if (unique(colparts(x)) != unique(rowparts(y)))
    incompatible_args("'%hu%'")
  
  # product
  result = blockmatrix(matrix(0, nrow(x), ncol(y)), 
                       rowparts(x), colparts(x))
  # from and to indices
  rows = from_to(rowparts(result))
  cols = from_to(colparts(result))
  
  for (i in 1:brow(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    for (j in 1:bcol(x)) 
    {
      # specified starting and ending indices
      jcols = cols$from[j]:cols$to[j]
      # extract block 'ij'
      X_ij = get_block(x, i, j)
      Y_ij = get_block(y, i, j)
      result[irows,jcols] = X_ij %*% Y_ij
    }
  }
  
  # output
  result 
}


