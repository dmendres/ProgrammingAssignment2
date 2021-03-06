## CacheMatrix provides two functions for managing a cached matrix inverse computation
### makeCacheMatrix creates the matrix cache, with an optional matrix
### cacheSolve solves for (and caches) the inverse of the matrix cached in its argument
### The cached value of the inverse is used if the matrix hasn't been updated, otherwise the value is NULL

## makeCacheMatrix creates a cache holding a matrix and its inverse, 
## with functions to manage the state of the cache

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


## cacheSolve returns the inverse of a matrix stored in cache x, possibly re-computing it if the cached matrix has been updated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInv()
  if (is.null(inv)) {
    x$setInv(solve(x$get()))
  }
  x$getInv()  
}

# #unit test
# m<-matrix(1:4,2,2)
# x <- makeCacheMatrix(m)
# all.equal(m,x$get())
# cacheSolve(x)
# all.equal(m,cacheSolve(makeCacheMatrix(cacheSolve(x))))
# 
# #non-trivial test
# x <- makeCacheMatrix(matrix(runif(100,max = 100), 10, 10))
# all.equal(cacheSolve(makeCacheMatrix(cacheSolve(x))),x$get())
