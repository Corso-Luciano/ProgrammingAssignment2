## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
#Sets the inv to Null
  inv <- NULL
#Function for setting a matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #function for retrieving the matrix 
  get <- function() x
  #function for setting the inverse 
  setInverse <- function(inverse) inv <<- inverse
  #function for getting the inverse 
  getInverse <- function() inv
  #list of functions 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix, retrieving it from cache if already calculated
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
