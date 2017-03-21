## These functions make it possible to calculate the inverse of a Matrix, then
## store the result in a cache, so that the result can later be found in the 
## cache (no need to calculate it again).

## This first function, makeCacheMatrix,
## can be used to create and store a matrix, and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This second function, cacheSolve, can retrieve the inverse that is stored in 
## the environment of makeCacheMatrix, so that it is not necessary to 
## calculate the inverse again if the matrix has not changed.



cacheSolve <- function(x, ...) {
                inverse <- x$getinv()
                if(!is.null(inverse)) {
                        message("getting cached data")
                        return(inverse)
                }
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinv(inverse)
                inverse
        }
