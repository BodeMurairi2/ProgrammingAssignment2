## These two functions work together to cache the inverse of a matrix.
## Because matrix inversion can be costly, caching the result lets us
## avoid recomputing the inverse when the matrix has not changed.

## makeCacheMatrix creates a special "matrix" object: really a list of
## functions that let us set/get the matrix and set/get its cached inverse.
## The inverse is stored in 'inv', which lives in this function's
## environment and is shared with the helper functions via the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" made by
## makeCacheMatrix. It first checks whether the inverse has already been
## cached; if so, it returns the cached value and skips the computation.
## Otherwise it solves for the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
