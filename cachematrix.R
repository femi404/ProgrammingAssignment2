## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## used by cacheSolve to get or set the inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
## stores the cached value
## initialize to NULL 
     inv <- NULL
     
## create the matrix in the working environment     
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## get the value of the matrix  
  get <- function() x
  
## invert the matrix and store in cache  
  setInverse <- function(inverse) inv <<- inverse 
  
## get the inverted matrix from cache  
  getInverse <- function() inv
  
## return the created functions to the working environment  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
## return inverted matrix from cache if it exists
## else create the matrix in working environment  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
## create matrix 
  mat <- x$get()
  inv <- solve(mat, ...)
  
## set inverted matrix in cache
  x$setInverse(inv)
  inv
}

#Test functions
Check <- diag(5,3)
Check

CachedMatrix <- makeCacheMatrix(Check)
cacheSolve(CachedMatrix)

Confirm <- diag(3,4)
Confirm

CachedMatrix <- makeCacheMatrix(Confirm)
cacheSolve(CachedMatrix)

cacheSolve(CachedMatrix)
