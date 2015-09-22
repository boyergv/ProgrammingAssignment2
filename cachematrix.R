
# This function aims to take a potentially time-consuming computation and make it much faster by
# caching the value of the inverse of a matrix, rather than making us resolve the inverse every
# time that the inverse of the matrix is required. 

# Create a Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMatrix <- function() x
    setInverseMatrix <- function(inverse) inv <<- inverse
    getInverseMatrix <- function() inv
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


## Create function cacheSolve

# Take the inverse of the matrix from makeCacheMatrix. If the inverse has already been
# calculated, then get the inverse from the cache instead of recomputing it.
# This program saves computational resources and time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverseMatrix()
    if(!is.null(inv)) {
        message("getting cached data... one moment please")
        return(inv)
    }
    data <- x$getMatrix()
    inv <- solve(data)
    x$setInverseMatrix(inv)
    inv
}