## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
