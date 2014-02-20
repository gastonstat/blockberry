\name{is.multiblock}
\alias{is.multiblock}
\title{Test if x is multiblock}
\usage{
  is.multiblock(x)
}
\arguments{
  \item{x}{an R object}
}
\description{
  If an object has blockdim return TRUE, otherwise FALSE
}
\examples{
# a blockvector
x = 1:10
bx = blockvector(x, parts=c(5,5), dims=2)

# test if multiblock
is.multiblock(x) # FALSE
is.multiblock(bx) # TRUE
}
\seealso{
  \code{\link{blockdimension}}
}
\keyword{multiblock}

