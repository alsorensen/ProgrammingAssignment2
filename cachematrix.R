## makeCacheMatrix: This function creates a special "matrix" object that can cache the inverse of a matrix.
##
## makeCacheMatrix creates a special "vector", which is a list containing a function to
##
##      1.set the value of the vector
##      2.get the value of the vector
##      3.set the value of the Cached Inverse Matrix
##      4.get the value of the Cached Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
