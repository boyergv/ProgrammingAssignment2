
# This function aims to take a potentially time-consuming computation and make it much faster by
# caching the value of the inverse of a matrix, rather than making us resolve the inverse every
# time that the inverse of the matrix is required. 
# "makeCacheMatrix" makes a list with a function that sets the value of the matrix, 
# gets the value of the matrix, sets the inverse value of the matrix, and gets the inverse
# value of the matrix.

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
    list(setMatrix=setMatrix,
        getMatrix=getMatrix,
        setInverseMatrix=setInverseMatrix,
        getInverseMatrix=getInverseMatrix)
}


## Create function cacheSolve

# "cacheSolve" checks to see if the inverse has already been computed
# If it has, it simply returns the inverse value of a matrix and doesn't bother performing
# the computation necessary to solve the inverse of the matrix.
# If the inverse value is not already solved for that matrix, it calculates the inverse and
# sets the value in the cache matrix.

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
