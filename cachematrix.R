## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

'''The first function, makeCacheMatrix creates a special "matrix", 
which is really a list containing a function to
1. set the value of the matrix
2. get the value of the matrix
3. set the value of the matrix inversion
4. get the value of the matrix inversion'''

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatrix <- function(matrix) m <<- matrix
    getMatrix <- function() m
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## Write a short comment describing this function
## The following function calculates the inversion of the list
## created with the above function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setMatrix(m)
    m
}
