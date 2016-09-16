## Put comments here that give an overall description of what your
## functions do
# This functions are in charge of storing in Cache a matrix and it's inverse, 
# so the inverse is just calculated once so when it is needed again, it only 
# looks up the value in cache instead of computing again.

## Write a short comment describing this function
# makeCacheMatrix: Stores in cache the matrix and it's inverse. 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve: returns the calculated inverse of a matrix or
#             calculates the inverse if isn't calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
