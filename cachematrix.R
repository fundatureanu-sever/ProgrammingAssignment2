
## Creates a special "matrix" objects which provides functions for caching the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(y) {
    x <<- y
    inverse = NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse

  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function which takes as input a special "matrix" returned by makeCacheMatrix
## Retrieves the inverse from the cache if  available, otherwise it computes the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv))
  {
    message("Retrieving cached matrix")
    return(inv)
  }

  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}

