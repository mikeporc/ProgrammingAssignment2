## The following function creates a matrix that can cache its inv
##inv is set to null initially
## uses get and set to check if x is cached
## if it is it is then set 
## same thing happens with the inverses
## returns a list of functions for matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        list( set=set, get = get, set_inv = set_inv, get_inv=get_inv)
}


## uses solve function to compute the inverse of a matrix
## if this has already been done, it pulls the inverse from
## the previously cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data,...)
        x$set_inv(inv)
        return(inv)
}
