## makeCacheMatrix and cacheSolve work together to calculate
## the inverse of an invertible matrix and cache the inverse
## so that it is not unnecessarily calculated repeatedly

## makeCacheMatrix creates a special vector that wraps a 
## matrix and its cacheable inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the matrix stored in the special vector
## created by makeCacheMatrix and inverts it, caching the result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## actual inverse calculation occurs only if not already returned
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
