## Programming Assignment#2: R function to cache potentially time-consuming computations
## Calculating the inverse of matrix is time-consuming, so we want to reduce the time to do the inverse
## by caching the result so tha it can be used again.
## function makeCacheMatrix takes an input matrix and returns a list which essentially gets/sets mean into the 
## vector and retrieves them if they exist already. makeCacheMatrix has the following libraries that achieves the above:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

## the list x will be used in cacheSolve ()

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


## cacheSolve performs inverse of the special matrix only if it has not been calculated by makeCacheMatrix. 
## If the inverse is already calculated, then function gets from the cache

cacheSolve <- function(x, ...) {
        # takes the matrix from makeCacheMatrix and does the inverse
        inverse = x$getinverse()
        
        # has the inverse been calculated already?
        if (!is.null(inverse)){ 
                # inverse available. skip computation 
                message("getting cached data")
                return(inverse)
        }
        
        # if the inverse has not been calculated, then do the inverse
        mat.data = x$get()
        inverse = solve(mat.data, ...)
        
        # setinverse sets the value of the inverse in the cache 
        x$setinverse(inverse)
        
        return(inverse)
}
