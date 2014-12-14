## These two functions work as a pair. 

## makeCacheMatrix
## When this function is called, it is passed a matrix,
## and returns an object which stores that
## matrix, as well as a placeholder for the
## matrix's invertsion.

## cacheSolve
## When this function is called, it is passed an
## object previously created by makeCacheMatrix.
## If the object does not contain the inversion
## of the matrix stored in the makeCacheMatrix
## object, this function will compute the inversion,
## and store it in the makeCacheMatrix object.
## If cacheSolve is subsequently called with the
## same object, it will retrieve the inversion
## from the makeCacheMatrix object rather than
## recomputing it.
 

## This function takes a matrix x that is passed
## to it and stores it. It will also store the
## inversion of x when it is computed.

makeCacheMatrix <- function(x = matrix()) {
  inversex <- NULL
  get <- function() { x }
  set <- function(y) {
    x <<- y
    inversex <<- NULL
  }
  setInverse <- function(x) {
    inversex <<-- solve(x)
  }
  
  getInverse <- function() { inversex }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Thus function will compute the inverse of a matrix
## stored in an object created by makeCacheMatrix,
## and store the inversion in that object.
## If the same object is subsequently passed to 
## cacheSolve, it will return the stored inversion
## rather than recomputing it.
## passed to it and store it in makeCacheMatrix.


cacheSolve <- function(x, ...) {
     
     invert <- x$getInverse()
     if(!is.null(invert)) {
       message("returning cached data")
       return(invert)
     } else {
      data <- x$get()
      invert = x$setInverse(data)
      invert <- x$getInverse()
      return(invert)
    }
}
