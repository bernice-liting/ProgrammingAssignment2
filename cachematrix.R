## R Programming Assignment Week 3
## There are 2 functions here. The 1st function makeCacheMatrix creates a special 
## "matrix" object that can cache its inverse.
## The 2nd function cacheSolve computes the inverse of the special "matrix" created  
## with the 1st function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
