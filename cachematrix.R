# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMx <- NULL
        set <- function(y) {
                x <<- y
                invMx <<- NULL
        }
        get <- function() x
        setinvMx <- function(inverse) invMx <<- inverse
        getinvMx <- function() invMx
        list(set = set, get = get, setinvMx = setinvMx, getinvMx = getinvMx)
}


# The function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        invMx <- x$getinverse()
        if(!is.null(invMx)) {
                message("getting cached data")
                return(invMx)
        }
        data <- x$get()
        invMx <- mean(data, ...)
        x$setmean(invMx)
        invMx
}