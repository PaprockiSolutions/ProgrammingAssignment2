## The functions `makeCacheMatrix` and `cacheSolve` work together to cache the 
## inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 
## The matrix object will store both the matrix and its cached inverse. 
## It provides functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix". 
## It first checks if the inverse has already been calculated. 
## If it has, it retrieves the inverse from the cache, otherwise, 
## it calculates the inverse and stores it in the cache for future use.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}