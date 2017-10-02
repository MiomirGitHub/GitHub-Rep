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