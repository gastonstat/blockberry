#' @title Blockvector Norm
#' 
#' @description Computes the norm(s) of a blockvector
#'
#' @param x a blockvector
#' @return norm of x
#' @export
#' @examples
#' # numeric blockvector
#' bv = blockvector(1:10, c(4,6), 2)
#' bvnorm(bv)
bvnorm <- function(x) 
{
  if (!is.numeric(x))
    stop("\nbvnorm() requires a numeric blockvector")
  
  # get block partitions
  xparts = parts(x)
  # indexify
  index = unlist(mapply(rep, seq_along(xparts), xparts))
  # calculate norm for each block
  res = rep(0, length(xparts))
  for (i in 1:length(xparts)) {
    res[i] = sqrt(sum(x[index==i] ^ 2))
  }
  # output
  res
}
