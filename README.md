# blockberry

**blockberry** is an experimental [R](http://www.r-project.org/) package that provides a wireframe for working with multiblock objects. 


## Motivation

The main motivating question behind **blockberry** is: *how to handle multiblock data?*

Our approach for addressing the previous question is based on an extremely simple yet ingenuous concept: 
take into account the block structure by means of what we call *block-dimension*

Simply put, the *block-dimension* is implemented as an attribute in R objects that allows us 
to introduce a block structure.

## Installation

Since ```blockberry``` is an experimental in-progress package, its distribution is not on CRAN but on the github 
repository [https://github.com/gastonstat/blockberry](https://github.com/gastonstat/blockberry)

In order to install ```blockberry``` you need to use the function ```install_github()``` 
from the package ```devtools``` (remember to install it first). Type the following lines in your R console:

```ruby
# only if you haven't installed "devtools"
install.packages("devtools")

# load "devtools"
library(devtools)

# install "blockberry"
install_github('blockberry', username = 'gastonstat')

# load "blockberry
library(blockberry)
```

### Example with blockvector

How to create a blockvector
```ruby
# say you have a numeric vector
vnum = 1:10

# blockvector (partitioned in 3 blocks)
bnum = blockvector(vnum, parts = c(3, 2, 5), dims = 3)
```


### Example with blockmatrix

How to create a blockmatrix
```ruby
# say you have a numeric matrix
m = matrix(1:20, 4, 5)

# option 1) blockmatrix using arguments `rowparts` and `colparts`
bm1 = blockmatrix(m, rowparts = c(2, 2), colparts = c(3, 2))

# option 2) blockmatrix using arguments `parts` and `dims`
bm2 = blockmatrix(vnum, parts = c(2, 2, 3, 2), dims = c(2, 2))
```


Authors Contact
--------------

[Gaston Sanchez](http://gastonsanchez.com) (```gaston.stat at gmail.com```)

Mohamed Hanafi (```mohamed.hanafi at oniris-nantes.fr```)
