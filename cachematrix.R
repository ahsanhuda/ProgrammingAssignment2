## Put comments here that give an overall description of what your
## functions do

## Takes in a matrix and returns four functions that get or set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Nested Functions
    # Value of variable inv is scoped in the running environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) inv <<- Inverse
    getInverse <- function() inv
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Takes in a vector of four functions returned by makeCacheMatrix(x=matrix()) and returns the inverse of the matrix
## Does not compute the value of inv is found in cache

cacheSolve <- function(x, ...) {
        # Retrive value of inverse matrix from cache
        inv <- x$getInverse()
        # If value found in cache, function does not re-compute the value
        if(!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
	    }
	    data <- x$get()
	    inv <- solve(data, ...)
	    x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}
