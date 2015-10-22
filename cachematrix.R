## These functions help manage a matrix and its inverse.
## The Inverse of the matrix is calculated only once, when it is requried for the first time.
## The subsequent calls to get the inverse will return the stored inverse matrix.

## This R function creates an environment to store the matrix & its inverse and also functions
## to manage it. 
makeCacheMatrix <- function(x = matrix()) { # Accepts a matrix as input and stores the value in varibale x
    
    ## Assing null to the variable which will hold inverse, note this line will 
    ## only be run when when makeCacheMatrix is called first time with matrix as input
    inv <- NULL
    
    ## This function can be used to change the matrix in an instance of makeCacheMatrix.
    ## when a$set() called with a matrix input, the internal stored values are changed 
    ## and inv is reset to Null.
    ## The <<- assignment tries to find variable with the given name and if found
    ## it sets its value, otherwise it creates the variable in the local environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get returns an handle to a function which just returns the x which contains the matrix
    get <- function() x
    
    # sets the inverse matrix value to variable inv
    setinverse <- function(solve) inv <<- solve
    
    # hanlde to a function which returns the value stored in inv (Inverse matrix)
    getinverse <- function() inv
    
    # return as list, handles to the functions declared above which can be called.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Accepts an instance of the makeCacheMatrix and manages the caching of inverse matrix
cacheSolve <- function(x, ...) {
    
    ## Gets the current stored value of inv variable used in makeCacheMatrix
    ## If the inverse matrix is already calculated, then inv should have the Inverse matrix
    ## otherseise inv will be null.
    inv <- x$getinverse()
    
    ## checks to see if the inv contains null, if no just return the inv 
    ## which is the inverse matrix needed.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    message("Inverse needs to be computed")

    ## if inv is null, then we not calculated the inverse yet, we need to calcualte
    ## inverse matrix and store the value in inv 
    
    ## Get the matrix 
    data <- x$get()
    
    ## Calculate the inverse of that matrix
    inv <- solve(data, ...)
    
    ## This call sets/caches the inverse matrix
    x$setinverse(inv)
    
    ## Return the inverse matrix.
    inv
}
