
# Assignment 2 by Darshil K

# Functions to store a matrix and cache its inverse.

# To create a special "matrix" object that can cache its inverse...

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(a) {
    x <<- a
    invers <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inv) {invers <<- inv}
  getInverse <- function() {invers}
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# To compute actual inverse of matrix if it is not already cached...

cacheSolve <- function(x, ...) {
  
  invers <- x$getInverse()
  if (!is.null(invers)) {
    message("returning cached data")
    return(invers)
  }
  matrix <- x$get()
  invers <- solve(matrix, ...)
  x$setInverse(invers)
  invers
}