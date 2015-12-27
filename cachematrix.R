## These two functions store a matrix and cache the inverse of the matrix

#This function creates an object including a matrix and a list of
#functions to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix(data=numeric(), nrow, ncol)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}

#This function checks to see whether makeCacheMatrix has already cached
#something.  If so, it returns the cached data.  If not, it calculates the
#inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
