## This pair of function is created to cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
  
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then this function
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
        return(inv)
        }
        data <- x$get()
        m <- Inverse(data, ...)
        x$setInverse(inv)
        inv
}
