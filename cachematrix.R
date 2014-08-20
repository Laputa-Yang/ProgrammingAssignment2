## Make a cached matrix inversion version, which is a wrapper object of matrix object. 
## The function will cache the inverse result once calculated

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # record the cached inverse
    inverseMatrix <- NULL
    # record the original matrix
    setMatrix <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    # get the original matrix
    getMatrix <- function() x
    # record the cached inverse
    setMatInverse <- function(inv) inverseMatrix <<- inv
    # get the inverse matrix
    getMatInverse <- function() inverseMatrix
    # return the list of these four functions
    list( setMatrix = setMatrix, getMatrix = getMatrix,
          setMatInverse = setMatInverse, getMatInverse = getMatInverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inv <- x$getMatInverse()
    # If the inverse has already been calculated, 
    # then the cachesolve should retrieve the inverse from the cache.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # calculate the inverse and record the inverse
    mat <- x$getMatrix()
    inv <- solve(mat)
    x$setMatInverse(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
