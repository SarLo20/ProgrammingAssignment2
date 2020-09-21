## cache the inverse of a matrix because matrix inversion
## is usually a costly computation

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) matrix <<- solve
    getmatrix <- function() matrix
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" 
## returned by the function makeCacheMatrix
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve will retrieve the 
## inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getmatrix()
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setmatrix(matrix)
    matrix
}

## for testing
# testMatrix <- matrix(rnorm(4), nrow = 2, ncol = 2)
# cacheSolve(makeCacheMatrix(testMatrix))
# solve(testMatrix)
