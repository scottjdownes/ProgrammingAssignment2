## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is used to generate a list of convenient functions for inspecting 
## and changing the contents of a matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve provides a mechanism for returning the cached results of a solve
## operation. If s is null, we'll generate a new s by running solve against the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
