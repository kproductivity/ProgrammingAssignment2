## Inverting a matrix is usually a costly computation. Caching the inverse
## of a matrix may be sometimes better than computing it again.
## These functions allow to cache the inverse of a matrix.


## Creates a special "matrix" object which caches the inverse of a 'set' matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        get <- function() x
        
        ## Function that sets the inverse of the matrix.
        ## Assumes matrix is invertible.
        setinverse <- function(solve) m <<- solve
        
        ## Function that gets the inverse matrix.
        getinverse <- function() m
      
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by
## function `makeCacheMatrix`. If the inverse has already been
## calculated, then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Return stored value of the matrix if it was calculated before.
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ## Otherwise calculate and return inverse matrix 'm'.
        data <- x$get()         # Retrieve matrix.
        m <- solve(data, ...)   # Calculate inverse matrix.
        x$setinverse(m)         # Set up cache.
        m
}