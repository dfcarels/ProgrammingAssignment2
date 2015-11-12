## With the makeCacheMatrix function you can create a matrix object that 
## can cache its inverse. It contains a list of 4 functions: set, get, 
## setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set = set,                
             get = get,                
             setinverse = setinverse,  
             getinverse = getinverse)  
        
}


## The cacheSolve function computes the inverse of the matrix returned 
## by the makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve function should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        
        inv
        
}