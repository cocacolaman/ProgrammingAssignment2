## The first function creates a special "matrix" object that stores 
## a matrix and cache's its inverse. The second function calculates 
## the inverse of the  special "matrix" object, however it first checks 
## to see if the mean has already been calculated. If this is the case, 
## it retreives the inverse from the cache  and skips the computation. 
## Otherwise it calculates the inverse and stores it in the cache for 
## future retrieval.


## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix" 
## object, however it first checks to see if the mean has already 
## been calculated. If this is the case, it retreives the inverse 
## from the cache and skips the computation. Otherwise it calculates 
## the inverse and stores it in the cache for future retrieval.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  m
}