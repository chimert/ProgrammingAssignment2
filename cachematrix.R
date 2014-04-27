## Functions for creating cache inverse Matrix

## This function should create a cache of an inverse Matrix in the Global
## It takes a matrix as an input and creates a set of functions available
## to call or set that inverse matrix
## environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Function set, will specify the object
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Function get, will return the object saved by 'set'
        get <- function() x
        ## Function setsolve, will specify the value of the inverse matrix
        setsolve <- function(solve) m <<- solve
        ## Function getsolve, will return the value of the inverse matrix
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)  
}


## This function will return the inverse of 'x' if it is in the cache
## otherwise, calculate it and return the value, store it in the cache

cacheSolve <- function(x, ...) {
    
        
        m <- x$getsolve()
        ## If the value of m is in the cache, return it from memory
        if(!is.null(m)) {
                message("getting inverse matrix from cache")
                return(m)
        }
        ## If there is no cache, calculate it and return the value
        data <- x$get()
        m <- solve(data, ...)
        ## Set the value into the cache
        x$setsolve(m)
        m
}
