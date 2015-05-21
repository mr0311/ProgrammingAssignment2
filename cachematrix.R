## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector" which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <<- function(y) {
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


## cacheSolve first checks to see if the inerse has already been calculated
## if so it returns the inverse from the cache and skips the computation
## else if calculates the mean of the data and sets the value of the inverse in cache
## via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse();
    
    if(!is.null(m)) {
        message("getting catched data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m
}
