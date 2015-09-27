## Functions for caching the inverse of a square matrix
##
## Computing the inverse of a square matrix can be done with the solve function.
## For example, if X is a square invertible matrix, then solve(X) returns
## its inverse. Assume that the matrix supplied is always invertible.

## makeCacheMatrix creates a special "matrix", which can cache its inverse
## This function is based on the makeVector example function provided in the
## instructions for this assignment
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the inverse of the matrix
##   - get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Calculates the inverse of the special "vector" created with the
## makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. If it
## has, it gets the inverse from the cache and skips the computation. If it
## hasn't, it calculates the inverse of the data and sets the value of the
## inverse in the cache via the setInverse function.
## This function is based on the cacheMean example function provided in the
## instructions for this assignment

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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
