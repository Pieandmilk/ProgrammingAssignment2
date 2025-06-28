## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse to NULL
        
        set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse cache when new matrix is set
        }
        
        get <- function() x  # Return the matrix
        
        setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
        
        getinverse <- function() inv  # Retrieve the cached inverse
        
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
    
        if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)  # Return cached inverse if available
        }
        
        data <- x$get()
        inv <- solve(data, ...)  # Compute the inverse
        x$setinverse(inv)  # Cache the inverse
        inv
}
