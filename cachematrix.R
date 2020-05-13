## The functions makecachematrix and cachesolve will take input, sets the value,
## gets the value of Matrix and sets the value, gets the value of Inverse Matrix 
# and the function can cache the Inverse of a matrix.

## makeCacheMatrix will create a matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## Takes the matrix as an input
      invmatrix <- NULL
                                            ##Sets the value of the input matrix
      set <- function(y){
            x <<- y
            invmatrix <<- NULL
      }
      get <- function() x                  ## get the value of the matrix
      setInverse <- function(solveMatrix) invmatrix <<- solveMatrix  ##sets and
      getInverse <- function() invmatrix              ##gets the value of invertible matrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function will calculate the inverse of the matrix returned by the makeCacheMatrix
##function...If inverse is already calculated, then this nfunction will take the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invmatrix <- x$getInverse()
      if(!is.null(invmatrix)){               ## If inverse matrix is not Null this will return the invertible matix
            message("Getting cached data of Inversed Matrix")
            return(invmatrix)
      }
      data <- x$get()                     ## Otherwise , if the inverse matrix is not null ,it will invese the matrix and return it
      invmatrix <- solve(data)
      x$setInverse(invmatrix)
      invmatrix      
}
