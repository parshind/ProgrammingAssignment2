## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <- y
    inv <- NULL
  }
  
  get <- function() x
  
  getinv <- function() inv
  setinv <- function(yi ) {
    inv <- yi
  }
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv
  if(!is.null(inv))
  {
      message("Got inverse matrix from cache")
      return(inv)
  }
  
  m <- x$get()
  inv <- solve(m)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
