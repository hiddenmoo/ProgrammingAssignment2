## Package containing functions to manage caching of matrix data and its inverse.  Allows for 
## the inverse for a matrix to be calculated once and stored instead of calculated several times.

## Create a cache object that contains the matrix and its inverse.  The returned list contians 
## getters and setters for the matrix and its inverse.  The inverse is initialized to null and 
## is only calculated if needed.  Use the cacheSolve function with the returned list to access
## the inverse of the matrix.  

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
  get <- function() matrix
  setinv <- function(x) inverse <<- x
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Get the invserse of the matrix as represented in the cacheMatrix object from the makeCacheMatrix
## function.  Only will calculate the inverse if it has not already been calculated.  Extra parameters 
## are passed to the solve function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  new_inverse <- solve(data, ...)
  x$setinv(new_inverse)
  new_inverse
}
