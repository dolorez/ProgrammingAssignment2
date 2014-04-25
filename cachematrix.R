## This pair of functions is written co complete the peer reviewed programming assignment 2
## in the coursera.org course R Programming (rprog-002)
## These functions are used to cache the matrix inverses after computation to save the processing power
## inverse computation is a computationally-heavy process

## makeCacheMatrix takes as an arguments a matrix and sets up a mechanism for storing a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() { x }
  set.inverse <- function(in.i) { i <<- in.i }
  get.inverse <- function() { i }
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## cacheSolve takes as an input the output of makeCacheMatrix and returns the inverse
 # if the inverse had already been computed, it is being returned from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$get.inverse()
  if(!is.null(i)) {
    message("returning data from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set.inverse(i)
  i
}
