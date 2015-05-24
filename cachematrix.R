## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
        x <<- y         # the <<- is used to assign a value to an object to an environment that is different from the 
        i <<- NULL      # current environment
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # takes the matrix from makeCacheMatrix and does the inverse
        inverse = x$getinverse()
        
        # has the inverse been calculated already?
        if (!is.null(inverse)){ 
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse)
        }
        
        # if the inverse has not been calculated, then do the inverse
        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(inverse)
        
        return(inverse)
        ## Return a matrix that is the inverse of 'x'
}
