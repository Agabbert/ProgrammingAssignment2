## This R code creates two functions required for the Week 2 assignment
## of R Programming.
## The first function creates a list of functions that is used as an input
## to the second function. The second function caches the inverse of a matrix

## Creates a list of functions to cache a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(invm) m <<- invm
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Uses output of makeCacheMatrix to cache the inverse of a matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}