## makeCacheMatrix and cacheSolve are two functions which
##      cache the inverse of a given matrix so long computations
##      can be averted

## makeCacheMatrix creates a special "matrix" object which 
##      can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve computes the inverse of a special "matrix" returned
##      by makeCacheMatrix above.  If the inverse has already been
##      calculated (and the matrix has not changed), then the 
##      cacheSolve should retrieve the inverse from the cache

cacheSolve<- function(x, ...){
        # Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}