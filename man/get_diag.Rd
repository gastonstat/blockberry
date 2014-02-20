\name{get_diag}
\alias{get_diag}
\title{Get blocks in diagonal}
\usage{
  get_diag(x)
}
\arguments{
  \item{x}{a blockmatrix}
}
\value{
  a block diagonal matrix
}
\description{
  Extracts those diagonal blocks from a given blockmatrix
}
\examples{
# create a blockmatrix
bv = blockvector(1:5, parts=c(2, 3))
B = bv \%*o\% bv

# blockmatrix with blocks in diagonal
get_diag(B)
}

