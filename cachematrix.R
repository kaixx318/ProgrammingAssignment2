## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialise matrix
  inverse <- NULL
  
  ## set matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## set inverse of matrix
  setInverse <- function(solveMatrix) inverse <<- solveMatrix
  
  ## get inverse of matrix
  getInverse <- function() inverse
  
  ## return functions as list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get inverse
  inverse <- x$getInverse()
  
  ##if inverse exist, retrieve inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## else get data
  data <- x$get()
  
  ## inverse matrix
  inverse <- solve(data, ...)
  
  ## set inverse
  x$setInverse(inverse)
  
  ## return inverse
  inverse
}
