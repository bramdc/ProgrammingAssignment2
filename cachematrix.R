## These functions provide a caching mechanism to calculate the inverse of a matrix 
## and store it so it does not have to be calculated each time it is required


## This function generates a "cache matrix" list containing functions 
## to retrieve/store cache values of a matrix and its inverse
## intended to be used with cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL
 
    set <- function(y) {
        ## when updating the source matrix, 
        ## also set the inverse to NULL (clear cache), 
        ## so the inverse will automatically be recalculated
        x <<- y
        i <<- NULL
    }
  
    get <- function() x ## returns the source matrix
    setinverse <- function(inverse) i <<- inverse ## set the result
    getinverse <- function() i ## returns the result
  
    ## returns the inner functions as a list to be accessible in parent environment
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
    
}



## calculates the inverse of a matrix, or returns the cache value if it has been calculated before
## x should be a "cache matrix" created through function makeCacheMatrix()

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    ## if a result is already set, don't continue to calculation but return the cached value
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get() ## get source matrix
    
    i <- solve(data, ...) ## calculate inverse
    x$setinverse(i) ## update the result in the "cache matrix"
    
    i ## return the result
}
