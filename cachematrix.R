## This function creates a special "matrix" object that can cache its inverse.
## This is a list containing functions to get/set a matrix and get/set a
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse of a matrix has already been
## calculated (and the matrix has not changed), then `cacheSolve` should
## retrieve it from the cache and skips the computation. Otherwise, it
## calculates the inverse of a matrix and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        ## Calculates the inverse of matrix as it is not in cache
        data <- x$get()
        inv <- solve(data, ...)
        ## Set calculated data in cache
        x$setinverse(inv)
        inv
}