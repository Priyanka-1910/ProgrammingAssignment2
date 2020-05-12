## Code to find the inverse of a matrix and cache it


##1. Code to initialise objects,create getter and setter functions
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                               ## Object initialization
        set <- function(y) {                                    ## sets the value of the objects per the formal argument
                
                x <<- y
                m <<- NULL
        }
        get <- function() x                                     ## Defines getter for the object X
        setinverse <- function(solve) m <<- solve               ## Defines the setter for inverse
        getinverse <- function() m                              ## Defines the getter for inverse
     
        list(set = set, get = get,                              ## Defines the list of the getters and setters
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
       
        m <- x$getinverse()                                     ## Calls getinverse function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                                   ## Calculates inverse of the matrix
        x$setinverse(m)
        m
}
