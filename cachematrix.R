## Coursera 
## R Programming
## Programming Assignment 2
## Create functions that use Scoping & Closure to cash potentially 
##      time-consuming computations


## makeCacheMatrix: This function creates a special "matrix" object that can 
##                      cache its inverse.
## return: object that cashes the inverse if it has been already calculated 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        getInverse <- function() inv
        setInverse <- function(inverse) inv <<- inverse 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already 
##              been calculated (and the matrix has not changed), then the 
##              cachesolve should retrieve the inverse from the cache.
## Return: A matrix that is the inverse of 'x'
##

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if (!is.null(inv)){                
                return(inv) # retrieve cashed data
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}