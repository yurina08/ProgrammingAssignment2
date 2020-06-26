## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following are a pair of functions that cache the inverse of a matrix.


## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y      ## set a new vector "y" as "x" in parent environment
        i <<- NULL   #reset "i"
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse  ## as i in parent environment
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i

}
