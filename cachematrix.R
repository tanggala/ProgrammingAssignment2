## Programming Assignment 2
## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## create null matrix
    inv <- NULL
    ## set value of "special" matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## retrieves value of "special" matrix
    get <- function() {
        x
    }
    ## saves the value of the inverse matrix
    setinverse <- function(inv_matrix) {
        inv <<- inv_matrix
    }
    ## fetches the value of the variable meant to store the inverse matrix
    getinverse <- function() {
        inv
    }
    
    ## creates the list of functions created by makeCacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    invMat <- x$getinverse()
    if(is.null(invMat)){ 
        ## if no inverse matrix has been calculated previously, calculate the 
        ## inverse and save it to cache
        data <- x$get()
        invMat <- solve(data)
        x$setinverse(invMat)
    } else {
        ## inverse matrix has already been calculated
        message("getting cached data")
    }
    ## return inverse matrix
    invMat
}
