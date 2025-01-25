## These functions cache the inverse of a matrix

## This function returns a list of 4 functions used to set and get the matrix and inverse from the cache

makeCacheMatrix <- function(x = matrix()){
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the cached inverse, if available, else calculates the inverse of the matrix and caches the result 

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
