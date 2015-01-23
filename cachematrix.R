## These functions invert a matrix and store this information in it's cache.
## If the matrix is inverted again the function checks if the matrix has changed.
##If it hasn't it returns the cached solution, otherwise it solves the new matrix.

## This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## This function calculates the inverse of the matrix created by the above function. 
## If the matrix hasn't changed it returns the cached inversion. If it has changed it calculated the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- solve(x)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
