\name{blockvector}
\alias{as.blockvector}
\alias{as.bvector}
\alias{blockvector}
\alias{is.blockvector}
\alias{is.bvector}
\title{Block Vector}
\usage{
  blockvector(data, parts = NULL, dims = NULL)

  is.blockvector(x)

  as.blockvector(x)
}
\arguments{
  \item{data}{a vector}

  \item{parts}{vector of partitions}

  \item{dims}{integer indicating the number of blocks}

  \item{x}{an R object}
}
\value{
  An object of class \code{"blockvector"}
}
\description{
  \code{blockvector} creates a block-vector from the given
  set of values \cr \code{as.bvector} attempts to turn its
  argument into a block-vector \cr \code{is.bvector} tests
  if its argument is a (strict) block-vector
}
\examples{
# some vectors
vnum = runif(10)
vlog = sample(c(TRUE, FALSE), size=10, replace=TRUE)
vstr = letters[1:10]

# convert vectors into block-vectors
bnum = blockvector(vnum, 10, 1)
blog = blockvector(vlog, c(5,5), 2)
bstr = blockvector(vstr, c(3,3,4), 3)

# test if objects are blockvectors
is.bvector(vnum)  # FALSE
is.bvector(bnum)  # TRUE

# generate a vector
v = 1:10

# convert 'v' into a block-vector (one block)
bv = as.bvector(v)
bv
}
\seealso{
  \code{\link{blockmatrix}}, \code{\link{vector}}
}

