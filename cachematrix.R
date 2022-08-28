

## This function creates a special "matrix" object that can cache its inverse. 
## This assignment operator "<<-" ensures values persist after the function call
## and are available later via the parent environment.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL            # set values
        }
        get <- function() x             # get value
        setinv <- function(inverse) inv <<- inverse     # set value of inverse matrix
        getinv <- function() inv                        # get value of inverse matrix
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" return by the makeCacheMatrix
## If the inverse has been calculated (and the matrix has not changed), then the cacheSolve 
## funciton will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                inv <- x$getinv()
        if (!is.null(inv)) {                            # check if inverse matrix is NULL
                message("getting cached data")          
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)                         # solves for inverse
        x$setinv(inv)
        inv
}
