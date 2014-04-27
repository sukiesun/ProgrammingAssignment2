## functions makeCacheMatirx and cacheSolve here   
## are able to cache potentially time-comsuming 
## computations to calculate inverted matrix

## makeCacheMatrix will get&set the matrix  
## and get&set the cache of the inverted matrix 
## if any

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }                              ## set the content of the matrix
        get <- function() x            ## get the content of the matrix
        setInverse <- function(Inverse) m <<- Inverse                            
                                       ## set the content of the inverted matrix
        getInverse <- function() m     ## get the content of the inverted matrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve will return the cache of the 
## inverted matrix if any and calculate the 
## inverse of matrix if no cache


cacheSolve <- function(x, ...) {
        m <- x$getInverse()            #query the x matrix's cache
        if(!is.null(m)) {              #if there is a cache
                message("getting cached data")
                return(m)              #print message and return cache value, no calculation is required
        }
        data <- x$get()                #if there is no cache for the matrix x
        m <- solve(data, ...)          #calculate the inverted matrix here
        x$setInverse(m)                #save the value to x's cache
        m                              #return the result
}
