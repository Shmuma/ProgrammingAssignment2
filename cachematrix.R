## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # cache of inverse
  inv <- NULL
  
  # replate original matrix, clears cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get original matrix
  get <- function() x
  
  # cache inverse
  setinv <- function(new_inv) inv <<- new_inv
  
  # get inverse
  getinv <- function() inv
  
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
