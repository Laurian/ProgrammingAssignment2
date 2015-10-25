## Caching matrix inverse


## Factory for a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  # inverse will be stored in
  cachedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  # get matrix
  get <- function() x
  
  # cache the inverse
  setInverse <- function(inverse) cachedInverse <<- inverse
  
  # get the cached inverse
  getInverse <- function() cachedInverse
  
  # returns the defined object with exposed methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## If no inverse is cached already, computes it and stores it in the cache

cacheSolve <- function(x, ...) {
  # get the cached inverse (will be null if not cached yet)
  inverse <- x$getInverse()
  
  # not cached?
  if (is.null(inverse)) {
    # inverse is not cached, compute and cache it:
    # get the matrix
    m <- x$get()
  
    # compute inverse
    inverse <- solve(m, ...)
    
    # cache it
    x$setInverse(inverse)
  }
  
  # return inverse
  inverse
}
