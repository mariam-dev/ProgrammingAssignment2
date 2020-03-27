# Assignment #2 Lexical Scoping
#
#   Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it 
#   repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions
#   that cache the inverse of a matrix.
#   Write the following functions:
#     1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#     2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#        If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the 
#        cache.
#   Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then 
#    solve(X) returns its inverse.
#   For this assignment, assume that the matrix supplied is always invertible.
#

makeCacheMatrix <- function(x = matrix()) {
  
  # 1. set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # 2. get the value of the matrix
  
  get <- function() x
  
  # 3. set the value of the matrix
  
  setmatrix <- function(matrix) m <<- matrix
  
  # 4. get the value of the matrix
  
  getmatrix <- function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
# It first checks to see if the inverse has already been calculated. If so, it gets 
# the inverse from the cache and skips the computation. Otherwise, it calculates it and
# sets its value to cache using the setmatrix function.
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inversem <- x$get()
  m <- solve(inversem, ...)
  x$setmatrix(m)
  m
}
