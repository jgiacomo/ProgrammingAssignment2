## These functions create a cached matrix and cached inverse of the matrix so
## that the inverse does not need to be recomputed everytime it is needed.

## Creates the cached variables to store the matrix and inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Checks to see if the inverse of the matrix, x, is already stored and returns
## it if it is. If the inverse has not been cached it will compute the inverse
## and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
