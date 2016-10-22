## The functions below cache and compute the inverse of a matrix.
## It creates the "matrix" object and caches its inverse

makeCacheMatrix <- function(mtrx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtrx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtrx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## Function computes the inverse of the matrix returned by makeCacheMatrix.
## The cacheSolve retrieve the inverse from cache if the inverse has already
## been calcuated.


cacheSolve <- function(mtrx, ...) {
    inverse <- mtrx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverse)
    return(inverse)
}