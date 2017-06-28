## makeCacheMatrix creates a list object that contains functions that assist with the inverting of the matrix
### that is provided as an argument
## cacheSolve will take the output of makeCacheMatrix and cache the inverse of the matrix

## This function creates an object which helps us cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() {x}
  setInverse <- function(m) {
    mat <<- m
    }
  getInverse <- function() {mat}
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## inverts the matrix and caches the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setInverse(mat)
  mat
  
}

