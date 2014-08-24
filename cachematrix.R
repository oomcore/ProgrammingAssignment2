## Two functions that can be used to invert an invertible matrix and cache the result
## cacheSolve performs inversion and stores result in list data structure previously
## created by makeCacheMatrix

## creates a list of 4 functions to set and get matrix, and set and get matrix inverse
## only storage and retrieval, no calcs
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setM <- function(y) {
    x <<- y
    i <<- NULL
  }
  getM <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(setM = setM, getM = getM,
       setInverse = setInverse,
       getInverse = getInverse)
}


## solves matrix and returns inverse after storing, or returns cached value if it exists
cacheSolve <- function(x, ...) {
  ## Return cached matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## nothing in cache - so calculate and return inverse of 'x'
  matr <- x$getM()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
