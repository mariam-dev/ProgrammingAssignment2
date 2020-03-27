## Put comments here that give an overall description of what your
## functions do

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
