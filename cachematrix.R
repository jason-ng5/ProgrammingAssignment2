## These functions create an object that can cache its matrix inverse and 
## retrieves / calculates the inverse.

## This function creates a special "matrix" object that can cache its inverse
## It creates a list containing functions that sets the value of the matrix, 
## gets the value of the matrix, sets the inverse of the matrix, 
## gets the inverse of the matrix respectively.

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


## Returns the inverse of the matrix if it has already been calculated
## Otherwise calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
