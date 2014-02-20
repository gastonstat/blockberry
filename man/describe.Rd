\name{describe}
\alias{describe}
\title{Describe}
\usage{
  describe(x)
}
\arguments{
  \item{x}{an R object}
}
\description{
  Provide an overall description of an R object
}
\examples{
# four objects
some_vector = 1:10
some_matrix = matrix(1:20, 5, 4)
some_bvector = blockvector(1:10, c(5,5), 2)
some_bmatrix = blockmatrix(some_matrix, c(3,2), c(2,2))

# describe objects
describe(some_vector)
describe(some_matrix)
describe(iris)
describe(some_bvector)
describe(some_bmatrix)
}

