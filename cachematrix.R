## These functions cache the inverse of a matrix, assuming the matrix is invertible.

## This function creates a list of functions that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) m <<- inversion
        getinversion <- function() m
        list(set = set, get = get, 
             setinversion = setinversion,
             getinversion = getinversion)

}


## This function checks to see if the inverse of the matrix has already been calculated.
## If the inverse has already been calculated it is retrieved, otherwise it is calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinversion()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}
