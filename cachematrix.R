## CacheMatrix provides two functions for managing a cached matrix inverse computation
### makeCacheMatrix creates the matrix cache, with an optional matrix
### cacheSolve solves for (and caches) the inverse of the matrix cached in its argument
### The cached value is used if the matrix hasn't been updated, and the 

## Creates a cache holding a matrix and its inverse, with functions to manage the state of the cache

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) mInv <<- inv
  getInv <- function() mInv
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Returns the inverse of a matrix stored in cache x, possibly computing it if the cached matrix has been updated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInv()
  if (is.null(inv)) {
    x$setInv(solve(x$get()))
  }
  x$getInv()  
}

#unit tests
x <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(x)
cacheSolve(makeCacheMatrix(cacheSolve(x)))
m <- matrix()

#non-trivial test
x <- makeCacheMatrix(matrix(runif(100,max = 100), 10, 10))
all.equal(cacheSolve(makeCacheMatrix(cacheSolve(x))),x$get())