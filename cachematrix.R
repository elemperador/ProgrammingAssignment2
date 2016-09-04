## The following functions can be used to cache the computation of a matrix inverse.

## Create an object to cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## The following function returns a previously calcualted inverse of a matrix
## which was stored in an object created with the function makeCacheMatrix.
## In case the inverse has not been calculated, the inverse is calculated
## and the result is cached.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  mInv <- solve(data)
  x$setinverse(mInv)
  mInv
}
