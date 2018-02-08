## ******************************************************************************************
## This file contains functions specific to the Programming Assignment 2 
## The below pair of functions will cache the inverse of a matrix
## ---------------------------------------------------------------------
##
## Sample usage:
## ---------------------------------------------------------------------
## >   source("cachematrix.R")      -- load the R code
## >   x <- matrix(rnorm(25), 5, 5) -- create a random 5x5 matrix 
## >   i <- makeCacheMatrix(x)      -- cache the matrix and it's inversion
## >   i$get()                      -- output the original matrix
## >   cacheSolve(i)                -- output the inverted matrix
## >   cacheSolve(i)                -- if run again, will get the cached version
## ---------------------------------------------------------------------
## >   x <- matrix(rnorm(25), 5, 5) -- create another matrix
## >   i$set(x)                     -- this will store the new version and "invalidate" the inversion cached
## >   cacheSolve(i)                -- will return the new inverted version anc cache it
## >   cacheSolve(i)                -- will return the newly cached inversion
## ******************************************************************************************


## The function makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         getinverse = getinverse,
         setinverse = setinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data for matrix inversion")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
