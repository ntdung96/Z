## The project will return the inverse of an invertible matrix..
## The result is cached so that it can be used in future computation.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {i <<- inverse}
      getInverse <- function() i
      list(set = set, get = get, setInverse = setInverse,
           getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by the function
## above. If the inverse is calculated previously, this cacheSolve function
## will retrieve the result from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)) {
            message("data retrieved from cache")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}