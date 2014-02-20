# blockberry

**blockberry** is an experimental [R](http://www.r-project.org/) package that provides a wireframe for working with multiblock objects. 


## Motivation

I developed `blockberry` to have a set of functions for testing objects in a friendly way, following the so-called [literate programming](http://www-cs-faculty.stanford.edu/~uno/lp.html) paradigm.

Without `blockberry`: If you want to test if a number is positive, you would do something like this:
```ruby
number = 10
if (number > 0) TRUE else FALSE
```

With `blockberry`: There is nothing wrong with the previous way of doing things. However, I still wanted to be able to type something like this:
```ruby
# another way
is_positive(number)
```
This is what `blockberry` offers you. By having functions like `is_positive()`, it helps your code to be more understandable. The underlying principle is to have tools that get you closer to the literate programming paradigm. That was my purpose for developing `blockberry`


## Installation

Since ```blockberry``` is an experimental in-progress package, its distribution is not CRAN but in github 
at the repository [https://github.com/gastonstat/blockberry](https://github.com/gastonstat/blockberry)

In order to install ```blockberry``` you need to use the function ```install_github()``` 
from the package ```devtools``` (remember to install it first):

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

# create a blockmatrix using arguments `rowparts` and `colparts`
bm1 = blockmatrix(m, rowparts = c(2, 2), colparts = c(3, 2))

# create a blockmatrix using arguments `parts` and `dims`
bm2 = blockmatrix(vnum, parts = c(2, 2, 3, 2), dims = c(2, 2))
```


Authors Contact
--------------

[Gaston Sanchez](http://gastonsanchez.com) (```gaston.stat at gmail.com```)

Mohamed Hanafi (````mohamed.hanafi at oniris-nantes.fr```)
