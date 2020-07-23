## Function creates a special matrix object that can cache its inverse.
## This function creates  matrix and checks whether it is inverse or not.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
  message("getting cached data")
  return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
