## cachematrix -- Provides makeCacheMatrix() and cacheSolve()
## makeCacheMatrix is an object that holds a matrix and its
## inverse. cacheSolve takes a makeCacheMatrix object and
## returns its inverse, either from cache or calculation, if
## cached value is not available.

## makeCacheMatrix(), our base object

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If x is a matrix for which we've cached the inverse, return the cached
## copy, if not process the inverse, cache it and return it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
