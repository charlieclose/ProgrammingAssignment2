## These functions store a matrix and its inverse, and also
## compute the inverse based on a passed-in matrix.

## makeCacheMatrix produces a list object that holds a matrix 
## and its inverse. x is the matrix and m is the inverse.
## The returned list provide getters and setters to x and m.
makeCacheMatrix <- function(x = matrix()) {
    
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


## cacheSolve returns the inverse of the matrix.
## The parameter x is the list returned from the 
## makeCacheMatrix function. If the inverse has already
## been computed and cached in m, then m in returned
## immediately. If it has not yet been computed, it is computed
## and stored in m so that the next time this function is called
## it can return the value directly from m.

cacheSolve <- function(x) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m) ## Cache the inverse.
    m
}
