##These pair functions can cache the inverse of a
# matrix, saving computation time if already computed

## This function creates a special "matrix" object that
# can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function()
        x
    setinverse <- function(inverse)
        inv <<- inverse
    getinverse <- function()
        inv
    
    list(set=set,get = getmatrix,
         setinv = setinverse,
         getinv = getinverse)
}



## This function computes the inverse of the "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve retrieves 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)) {
        message("Getting cache inverse")
        return(m)
    } 
    message("Inverse Matrix not cached... Computing...")
    matrix <- x$get()
    m <- solve(matrix)
    x$setinv(m)
    m
}