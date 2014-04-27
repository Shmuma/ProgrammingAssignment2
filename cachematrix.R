## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # try to obtain inverse from data
  inv <- x$getinv()
  # if computed already, return cached result
  if (!is.null(inv))
    return (inv)
  # otherwise, obtain matrix
  m <- x$get()
  # invese it
  inv <- solve(m) 
  # cache result
  x$setinv(inv)
  # return
  inv
}
