## The pair of functions "makeCacheMatrix" and "cacheSolve" cache 
## the inverse of a matrix to make the computing less time-consuming.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) 
                {
                x <<- y
                inverse <<- NULL
                }
        get <- function() x
        setSolve <- function(solve) {inverse <<- solve}
        getSolve <- function() inverse
        list(set = set, get = get, 
             setSolve = setSolve,
             getSolve = getSolve)                
}


## This function computes the inverse of the matrix returned by 
## the function "makeCacheMatrix".
## if the inverse has already been calculated, then "cacheSolve"
## will retrieve the inverse cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getSolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setSolve(inverse)
        inverse
}
