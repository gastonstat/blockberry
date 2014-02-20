#' @title usual-hadamard product
#' 
#' @description 
#' usual-hadamard product for multiblocks
#' 
#' @param x a blockmatrix
#' @param y a blockmatrix
#' @usage x \%uh\% y
#' @seealso \code{\link{scalar_scalar}}
#' @export
#' @rdname usual_hadamard
#' @examples
#' # blockmatrices
#' A = blockmatrix(matrix(1:16, 4, 4), rowparts = 4, colparts = c(2, 2))
#' B = blockmatrix(matrix(1:16, 8, 2), rowparts = c(4, 4), colparts = 2)
#' 
#' A %uh% B
"%uh%" <- function(x, y) {
  usual_hadamard(x, y)
}


#' @rdname usual_hadamard
#' @S3method usual_hadamard default
#' @S3method usual_hadamard blockmatrix
usual_hadamard <- function(x, y) {
  UseMethod("usual_hadamard", x)
}

usual_hadamard.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%uh%'")
  if (!is.multiblock(x) | !is.multiblock(y))
    non_multiblock_args("'%uh%'")
}

usual_hadamard.blockmatrix <- function(x, y) 
{
  if (!is.multiblock(y))
    non_multiblock_args("'%uh%'")
  
  # block-matrix and block-vector
  if (is.bvector(y))
    invalid_args("'%uh%'")
  
  # block-matrix and block-matrix
  if (is.bmatrix(y))
    return(usual_hada_prod_bmatrix_bmatrix(x, y))
}




# private function
usual_hada_prod_bmatrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!identical(bcol(x), brow(y)))
    incompatible_args("'%uh%'")
  
  # check all identical block rows in 'x'
  if (length(unique(rowparts(x))) != 1)
    stop("\nblocks of different dimensions")
  # check all identical block columns in 'x'
  if (length(unique(colparts(x))) != 1)
    stop("\nblocks of different dimensions")

  # check all identical block rows in 'y'  
  if (length(unique(rowparts(y))) != 1)
    stop("\nblocks of different dimensions")
  # check all identical block colmuns in 'y'
  if (length(unique(colparts(y))) != 1)
    stop("\nblocks of different dimensions")
  
  # check rows compatibility between 'x' and 'y'
  if (unique(rowparts(x)) != unique(rowparts(y)))
    incompatible_args("'%uh%'")
  # check colmuns compatibility between 'x' and 'y'
  if (unique(colparts(x)) != unique(colparts(y)))
    incompatible_args("'%uh%'")
  
  
  # product
  result = blockmatrix(matrix(0, nrow(x), ncol(y)), 
                       rowparts(x), colparts(y))
  # from and to indices
  rows = from_to(rowparts(result))
  cols = from_to(colparts(result))
  
  for (i in 1:brow(x)) 
  {
    # specified starting and ending indices
    irows = rows$from[i]:rows$to[i]
    for (j in 1:bcol(y)) 
    {
      # specified starting and ending indices
      jcols = cols$from[j]:cols$to[j]
      # extract block 'ij'
      X_i = separate(rowblock(x, i))
      Y_j = separate(colblock(y, j))
      # sum of hadamard product between sub-blocks 
      tmp = 0
      for (h in 1:length(X_i)) {
        tmp = tmp + do.call("*", list(X_i[[h]], Y_j[[h]]))
      }
      result[irows,jcols] = tmp
    }
  }
  
  # output
  result 
}
