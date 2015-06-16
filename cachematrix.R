
makeCacheMatrix <- function(x = matrix()) {
    #check is a square matrix
    if(nrow(x) != ncol(x)) stop("Matrix must be square")
    #initialize empty inverse matrix
    inv <- NULL
    
    #declare setter function
    set <- function(y) {
        #check is a square matrix
        if(nrow(y) != ncol(y)) stop("Matrix must be square")
        #set internal variables values
        x <<- y
        inv <<- NULL
    }
  
    #define get internal matrix function
    get <- function() x
        
    #define get internal reverse matrix function
    getinv <- function() inv
    
    #define set internal reverse matrix function
    
    setinv <- function(yi ) {
        inv <<- yi
    }
    #return created functions to environment
    list(set = set, get = get, getinv = getinv, setinv = setinv)
  
}

# Declare cacheSolve function

cacheSolve <- function(x, ...) {
    #get inverse matrix from the objectc
    inv <- x$getinv()
    # Check if inverse matrix is already calculated
    if(!is.null(inv))
    {
        #show info message
        message("Got inverse matrix from cache")
        #return previous result
        return(inv)
    }
    
    #if inverse matrix not cached try to find id
    #get original matrix from the object
    m <- x$get()
    #solve reverse matrix for original
    inv <- solve(m)
    #call setter for reverse matrix
    x$setinv(inv)
    #return reverse matrix 
    inv

}



