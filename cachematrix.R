## makeCacheMatrix & cacheSolve are functions used in conduction to take the inverse of a matrix 
##and cache it


## makeCacheMatrix takes a matrix as its argument and has subfunctions to store and cache the inverse of the matrix
##the subfunctions are called in the cacheSolve function to return the 
##cached inverse if it exists or to cache the inverse if it isn't already in the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes makeCacheMatrix as its argument and either returns the cache of the matrix 
## if it exists or calculates the inverse and calls on the setinverse subfunction to store the 
## inverse in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
