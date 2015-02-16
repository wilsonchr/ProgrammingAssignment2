## The two functions contained here:
##    1. Create a matrix that is capable of caching the inverse for later use
##    2. Solve for the inverse of a matrix using the caching capabilities in #1

## The makeCacheMatrix function creates a new object that is capable of caching
## the original and inverse of a matrix once it's been solved.  The object is 
## initially created by providing a matrix object or by using the get/set
## functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function provides the inverse of the cacheable matrix x.  
## If the cacheable matrix x does not have the inverse computed, it will be
## computed, otherwise it will simply return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    # It's cached! No need to compute.
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
