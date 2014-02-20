\name{separate}
\alias{separate}
\title{separate}
\usage{
  separate(x)
}
\arguments{
  \item{x}{multiblock object}
}
\description{
  Separate a multiblock into blocks
}
\examples{
# block-vector
bnum = blockvector(runif(10), c(5,5), 2)

# separate bnum
separate(bnum)

# block-matrix
a = matrix(runif(20), 5, 4)
dimnames(a) = list(1:5, paste("x", 1:4, sep=''))
A = blockmatrix(a, rowpart=c(2, 3), colpart=c(2, 2))

# separate A
separate(A)
}

