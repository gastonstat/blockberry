#' @title Block-vector Outer Product
#' 
#' @description Outer product for (block)vectors.
#' 
#' @details This product requires compatible objects
#' @param x a block-vector
#' @param y a block-vector
#' @usage x \%*o\% y 
#' @export 
#' @rdname outerprod
#' @examples
#' # vector outer product
#' v = 1:5
#' v %*o% v
#' 
#' # block-vector outer product
#' bv = blockvector(v, parts=c(3, 2))
#' bv %*o% bv
"%*o%" <- function(x, y) {
  outerprod(x, y)
}


#' @rdname outerprod
#' @S3method outerprod default
#' @S3method outerprod blockmatrix
#' @S3method outerprod blockvector
#' @S3method outerprod matrix
#' @S3method outerprod numeric
outerprod <- function(x, y) {
  UseMethod("outerprod", x)
}

outerprod.default <- function(x, y) 
{
  if (!is.numeric(x) | !is.numeric(y))
    non_numeric_args("'%*o%'")
}

outerprod.blockmatrix <- function(x, y) 
{
  # no outer product for blockmatrics
  invalid_args("'%*o%'")
}

outerprod.blockvector <- function(x, y) 
{
  # block-vector and block-vector only
  if (!is.bvector(y)) {
    invalid_args("'%*o%'")
  } else {
    return(outerprod_bvector_bvector(x, y))
  }
}

outerprod.matrix <- function(x, y) 
{
  # matrix and non-multiblock
  if (is.multiblock(y))
    invalid_args("'%*o%'")
  x %o% y
}

outerprod.numeric <- function(x, y) 
{
  # vector and others non-multiblock
  if (is.multiblock(y)) invalid_args("'%*o%'")
  
  # vector and vector
  outerprod_vector_vector(x, y)
}



# private function
outerprod_bvector_bvector <- function(x = NULL, y = NULL) 
{
  if (!same_parts(x, y)) incompatible_args("'%*o%'")
  
  outer_xy = tcrossprod(x, y)
  # output
  blockmatrix(outer_xy, parts(x), parts(y))
}

# private function
outerprod_vector_vector <- function(x = NULL, y = NULL) 
{
  if (length(x) != length(y)) incompatible_args("'%*o%'")
  
  # output
  tcrossprod(x, y)
}
