## makeCacheMatrix is a function that receives a matrix as input and returns a
## special matrix object with methods to:
## 1. get the value of the matrix
## 2. set the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
##
## cacheSolve is a function that calculates the inverse of the matrix. In case
## the inverse has been calculated it returns the cached result.


## Returns a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix_inverse <<- inverse
    getinverse <- function() matrix_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Computes the inverse of the matrix object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data.")
        return(inverse)
    }
    data <- x$get()
    # Assume the matrix is always invertible.
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
