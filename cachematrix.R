####################################################################
## These two stubs will compute and cache the inverse of a matrix.
## This function will avoid the computation again if it has been 
##     calculated already.
####################################################################

## this function will perform the following
##    a) get and set the value of a matrix
##    b) get and set the value of inverse of a matrix 
##    c) cache the values

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the value
  inv <- NULL
  
  ## set the value of a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of a matrix
  get <- function() x
  
  ## set the value of inverse of a matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the value of inverse of a matrix
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function will check if the value is computed already. If so,
## get the result else compute the inverse of a matrix. After computation
## store the value in the cache

cacheSolve <- function(x, ...) {
  
  ## get the value from cache
  inv <- x$getinverse()
  
  ## if value exists then return the value
  if(!is.null(inv)) {
    message("getting cached data!!!")
    return(inv)
  }
  
  ## if not, get the value of a matrix
  data <- x$get()
  
  ## compute the inverse of a matrix using built-in function
  inv <- solve(data)
  
  ## store the value into cache
  x$setinverse(inv)
  
  ## return the computed value
  inv
}