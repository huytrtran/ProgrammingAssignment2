## This script contains two functions for getting the inverse of an invertible 
##  square matrix. The makeCacheMatrix function creates an object containing
##  a matrix and functions that operate on that matrix. The cacheSolve function
#   gets the inverse of a matrix from the cache or calculates the inverse if
##  it does not exist in the cache.
## -----------------------------------------------------------------------------

## This function creates an object containing a matrix and list of functions 
##  that can operate on that matrix. The function assumes the input matrix
##  is square and invertible.
##  - the set function updates the value of the matrix in the object
##  - the get function returns the current value of the matrix
##  - the setInverse function updates the value of the inverse of the matrix
##  - the getInverse function returns the cached value of the inverse of the 
##      matrix (returns a null if the inverse of the current matrix has not been 
##      calculated yet)
makeCacheMatrix <- function(x = matrix()) {
    
    ## initializes the inverse of the matrix as null
    inv <- NULL
    
    ## creates a function to set a new value for the matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        inv <<- NULL
    }
    
    ## creates a function to get (return) the matrix
    get <- function() x
    
    ## creates a function to set (cache) the inverse of the matrix
    setInverse <- function(matrixInverse) inv <<- matrixInverse
    
    ## creates a function to get (return) the cached inverse of the input matrix
    getInverse <- function() inv
    
    ## outputs a list of enclosed functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
## This function gets the inverse of the matrix contained in the object x (where
##  x is created by the makeCacheMatrix function)
cacheSolve <- function(x, ...) {
    
    ## gets the currently cached inverse of the matrix in x
    inv <- x$getInverse()
    
    ## IF the inverse of the matrix is cached (i.e. not null) THEN leave inv as 
    ##  the cached value
    ## ELSE calculate the inverse of the matrix and cache the value
    if(!is.null(inv)) {
        message("getting cached inverse")
    } else {
        calculateMatrix <- x$get()
        inv <- solve(calculateMatrix)
        x$setInverse(inv)
    }
    
    ## returns the inverse
    inv
}
