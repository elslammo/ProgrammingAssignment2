## Two matrix helper functions: 
## 1) Creates a matrix with named values for getting and setting both the matrix itself and its inverse
## 2) Returns the inverse of a matrix created with function #1, including caching functionality 

## Creates named getters and setters for a given matrix
## and also for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverted) inverse <<- inverted
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## Returns the inverse of a matrix. First checks whether the value has
## already been calculated and is cached. Otherwise, calculates the
## inverse and sets it in the cache before returning.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
