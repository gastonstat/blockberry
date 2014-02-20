#' @title usual-usual product
#' 
#' @description 
#' usual-usual product for multiblocks
#' 
#' @details This product requires objects to have compatible
#' block-dimensions
#' 
#' @param x a numeric multiblock
#' @param y a numeric multiblock
#' @usage x \%uu\% y
#' @seealso \code{\link{scalar_scalar}}
#' @export
#' @rdname usual_usual
#' @examples
#' # block vector
#' v = 1:5
#' bv = blockvector(v, parts=c(2, 3))
#' bv %uu% bv
#' 
#' # matrix
#' a = matrix(1:20, 4, 5)
#' rownames(a) = paste("ind", 1:4, sep='')
#' colnames(a) = c("a1", "a2", "b1", "b2", "b3")
#' 
#' # block-matrix and block-matrix
#' A = blockmatrix(a, c(2, 2), c(2, 3))
#' A %uu% t(A)
#' 
#' # block-matrix and block-vector
#' A %uu% bv
"%uu%" <- function(x, y) {
  usual_usual(x, y)
}


#' @rdname usual_usual
#' @S3method usual_usual default
#' @S3method usual_usual blockvector
#' @S3method usual_usual blockmatrix
usual_usual <- function(x, y) {
  UseMethod("usual_usual", x)
}

usual_usual.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%uu%'")
  if (!is.multiblock(x) | !is.multiblock(y))
    non_multiblock_args("'%uu%'")
}

usual_usual.blockmatrix <- function(x, y) 
{
  if (!is.multiblock(y))
    non_multiblock_args("'%uu%'")
  
  # block-matrix and block-vector
  if (is.bvector(y))
    return(usual_usual_prod_bmatrix_bvector(x, y))
  
  # block-matrix and block-matrix
  if (is.bmatrix(y))
    return(usual_usual_prod_bmatrix_bmatrix(x, y))
}

usual_usual.blockvector <- function(x, y) 
{
  # block-vector and block-vector
  if (!is.bvector(y)) {
    non_multiblock_args("'%uu%'")
  } else {
    return(usual_usual_prod_bvector_bvector(x, y))
  }
}




# private function
usual_usual_prod_bmatrix_bmatrix <- function(x = NULL, y = NULL) 
{
  if (!identical(colparts(x), rowparts(y)))
    incompatible_args("'%uu%'")
  
  xy = x %*% y
  as.blockmatrix(xy, rowparts(x), colparts(y))  
}


# private function
usual_usual_prod_bmatrix_bvector <- function(x = NULL, y = NULL) 
{  
  if (!identical(colparts(x), parts(y)))
    incompatible_args("'%uu%'")
  
  xy = as.numeric(x %*% y)
  blockvector(xy, rowparts(x))
}


# private function
usual_usual_prod_bvector_bvector <- function(x = NULL, y = NULL) 
{  
  if (!same_parts(x, y))
    incompatible_args("'%uu%'")
  
  sum(x * y)
}
