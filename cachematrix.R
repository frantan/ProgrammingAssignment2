##  Cache the inverse of a matrix, assuming it is invertible

##  'makeCacheMatrix' takes as input a matrix and create a list containing functions to
##  -set the value of a matrix in the cache
##  -get the value of a matrix from the cache
##  -set the value of the inverse of a matrix in the cache
##  -get the value of the inverse of a matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverted) m <<- inverted  
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## 'cachesolve' takes as input the output of 'makeCacheMatrix(x)' and returns the inverse of x. 
## If the inverse has already been computed, it gets it from the cache and skips its computation.
## Otherwise computes the inverse and sets its value in the cache

cacheSolve <- function(x, ...) {
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
