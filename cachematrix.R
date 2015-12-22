##R Programming - Assignment 2
##Two functions that cache and return the inverse of a matrix.

## makeCacheMatrix
## Inputs: 1 Matrix
## Returns: 1 List of 4 functions:  set, get, setinverse and getinverse
  
  makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv 
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
  }


## CacheSolve
## Works in conjunction with makeCacheMatrix to solve and set the matrix inverse.
## Inputs:  the initialized list from makeCacheMatrix
## Returns:  the solved matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinverse()
  if(!is.null(I)) {
      message("getting cached data")
      return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
  
}
