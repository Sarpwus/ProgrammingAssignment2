## This is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special  "matrix" that
## "makecacheMatrix" returns.

## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse of the matrix")
                return(i)
        }
        invmatrix <- x$get()
        i <- solve(invmatrix, ...)
        x$setInverse(i)
        i
}
