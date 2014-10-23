## This file contains two functions that, respectively, do the following:
## 1. Creat, get, and set a matrix and its inverse.
## 2. Computed and return inverse of a matrix, or a cached copy of the inverse.

## The "makeCacheMatrix" function creates a matrix, and defines accessor and mutator 
## functions for that matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(cacheSolve) i <<- cacheSolve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The "cacheSolve" retuns the inverse of matrix x. If the inverse is previously 
## computed and has not been modified, it retuns a cached copy of the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

