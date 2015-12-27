##########################################################
# Filename      : cachematrix.R
# Purpose       : The functions in this file will 
# Functions     : This file contains the following
#                 * makeCacheMatrix
#                   Creates the "special" matrix object
#                   containing the cached inverse matrix
#                   and the functions to set and get its
#                   content
#                 * cacheSolve
#                   Uses the "special" matrix to retrieve
#                   the cached version of the inversed
#                   matrix. If it's empty, this function
#                   will calculate the inversed matrix
# Version       : v1.0
##########################################################

##########################################################
# Function      : makeCacheMatrix
# Parameter(s)  : x, a matrix that can be inversed
# Purpose       : This function creates a special "matrix" object 
#                 that can cache its inverse.
##########################################################

makeCacheMatrix <- function(x = matrix()) {
    
    # inv will contain the inversed matrix
    # and is set to NULL to make sure it in scope 
    # for the following functions
    inv <- NULL
    
    # the set function sets the (source) matrix
    # to be inversed and resets the 
    # inversed matrix to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # the get function returns the (source) matrix value
    get <- function() x
    
    # the setinverse function takes the inversed matrix
    # as a parameter and stores (caches) it into inv
    setinverse <- function(par_inv) inv <<- par_inv
    
    # the getinverse function returns the inversed matrix
    # inv 
    getinverse <- function() inv
    
    # a list of the created functions is returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    
}


##########################################################
# Function      : cacheSolve
# Parameter(s)  : x, an instance of the "special" matrix
#                 object, created by makeCacheMatrix
# Purpose       : This function uses the "special" matrix
#                 and checks whether there is a cached
#                 version of the inversed matrix. If so,
#                 it returns the cached invered matrix.
#                 Otherwise it calculates the inverse
#                 using the solve() function
##########################################################

cacheSolve <- function(x, ...) {
    # Read the cached version of the inversed matrix
    invmat <- x$getinverse()
    
    # If the cached version isn't empty return it
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    
    # Else read the "source" matrix into data
    data <- x$get()
    
    # Calculate the inverse matrix...
    invmat <- solve(data, ...)
    
    # ... and cache it into the "special" matrix
    x$setinverse(invmat)
    
    # Finally it returns the inverse matrix
    invmat
    
}
