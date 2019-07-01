## Introduction: 

## The cachematrix.R file is composed of two functions, makeCacheMatrix() 
## which creates an R object that stores a matrix and its inverse and
## cachesolve()  which takes as argument the object returned by the 
## previous funtion, calculates its inverse and stores its value in the 
## the makecacheMatrix() function

## Description of makecacheMatrix:

## The function makecacheMatrix() takes only one argument, "x", a matrix.
## Once the function is ran, it creates an object "inv.matrix" (the inverse ## of the matrix "x") and set its value to NULL. The set() function takes ## an as argument "y" that once the function makecacheMatrix is ran for the ## first time, assign its value to "x" in the parent environment (<<-  
## operator), without running the makecacheMatrix function again. The 
## second function inside makecacheMatrix is the "getter" function, which
## defines the matrix to be inverted and saves its value. The third
## function is the setter which takes the argument Z that will be created
## by the cachesolve function. The last function is the getter of the
## inverse matrix, this function is the where the inverted
## matrix is saved once the function cachesolve is ran. The last part of
## makecacheMatrix() creates a list inside which each function defined
## inside makecacheMatrix() is assigned as an element and returns it to the
## parent environment. 

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


## Description of cachesolve

## The function cachesolve() takes as argument the object created by 
## makecacheMatrix(). The first part of the function retrieves the inverse
## matrix from cache ( return(inv.matrix)), if the function has already
## been executed at least once. If not, then the "if statement" will
## evaluate as FALSE and the function will calculate the inverse of the
## matrix provided and store its value inside the set_inv.matrix() function ## inside the makecacheMatrix() function.


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

