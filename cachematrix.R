## These functions help manage a matrix and its inverse.
## The Inverse of the matrix is calculated only once, when it is requried for the first time.
## The subsequent calls to get the inverse will return the stored inverse matrix.
## This file is checked in github, SimbAris account

## Sample input and output
# > c=rbind(c(1, -1/4), c(-1/4, 1))  
# > c
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
# > m <- makeCacheMatrix(c)
# > m$getinverse()
# NULL
# > im <- cacheSolve(m)
# Inverse needs to be computed
# > im
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > m$getinverse()
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > im <- cacheSolve(m)
# getting cached data
# > im
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
# > m$getinverse()
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667


## This R function creates an environment to store the matrix & its inverse and also functions
## to manage it. 
makeCacheMatrix <- function(x = matrix()) { # Accepts a matrix as input and stores the value in varibale x
    
    ## Assing null to the variable which will hold inverse, note this line will 
    ## only be run when when makeCacheMatrix is called first time with matrix as input
    invrs <- NULL
    
    ## This function can be used to change the matrix in an instance of makeCacheMatrix.
    ## when a$set() called with a matrix input, the internal stored values are changed 
    ## and invrs is reset to Null.
    ## The <<- assignment tries to find variable with the given name and if found
    ## it sets its value, otherwise it creates the variable in the local environment
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    
    # get is an handle to a function which just returns the x having the actual matrix
    get <- function() x
    
    # sets the inverse matrix value to variable invrs
    setinverse <- function(solve) invrs <<- solve
    
    # Hanlde to a function which returns the value stored in invrs (Inverse matrix)
    getinverse <- function() invrs
    
    # Return as list, handles to the functions declared above which can be called.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Accepts an instance of the makeCacheMatrix and manages the caching of inverse matrix
cacheSolve <- function(x, ...) {
    
    ## Gets the current stored value of invrs variable used in makeCacheMatrix
    ## If the inverse matrix is already calculated, then invrs should have the Inverse matrix
    ## otherseise invrs will be null.
    invrs <- x$getinverse()
    
    ## checks to see if the invrs contains null, if no just return the invrs 
    ## which is the inverse matrix needed.
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    
    message("Inverse needs to be computed")

    ## If invrs is null, then we not calculated the inverse yet, we need to calcualte
    ## inverse matrix and store the value in invrs 
    
    ## Get the matrix 
    data <- x$get()
    
    ## Calculate the inverse of that matrix
    invrs <- solve(data, ...)
    
    ## This call sets/caches the inverse matrix
    x$setinverse(invrs)
    
    ## Return the inverse matrix.
    invrs
}
