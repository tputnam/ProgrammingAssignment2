## This is a solution for the programming assignment to write a function 
## makeCacheMatrix to create an invertible matrix, then write a second 
## function cacheSolve to invert the matrix and cache it.
## The cacheSolve returns the cached version of the inverted 
## matrix if it already exists and the matrix hasn't changed.


## The function makeCacheMatrix will create an invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix =  setmatrix,
             getmatrix = getmatrix)

}


## The function cacheSolve will invert the matrix created by makeCacheMatrix and
## cache the result.  If the cached version already exists, cacheSolve will
## return the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
