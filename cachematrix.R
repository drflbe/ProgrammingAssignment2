## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv.matrix <- NULL
        set <- function(y){
                x <<- y
                inv.matrix <<- NULL
                }
        get <- function() x 
        set_inv.matrix <- function(z) inv.matrix <<- z
        get_inv.matrix <- function() inv.matrix
        list(set = set, get = get, set_inv.matrix = set_inv.matrix, get_inv.matrix = get_inv.matrix)
        }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv.matrix <- x$get_inv.matrix()
        if(!is.null(inv.matrix)){
                message("Getting cached data")
                return(inv.matrix)
        }
        data <- x$get()
        inv.matrix <- solve(data,...)
        x$set_inv.matrix(inv.matrix)
        inv.matrix
        }
## Return a matrix that is the inverse of 'x'

