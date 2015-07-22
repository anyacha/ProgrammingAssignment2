## Programming Assignment 2 will take advantage of the scoping rules of R and 
## how they can be manipulated to preserve state inside of an R object.

#two functions that that cache the inverse of a matrix
#assumptions: assume that the matrix supplied is always invertible.

## fn1 makeCacheMatrix: creates a list of functions:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the matrix inverse
#4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inverse <-NULL
    set <- function(y) {
        x <<- y
        x_inverse <<-NULL
    }
    get <- function() x
    
    setinverse <- function(z) x_inverse <<- z
    getinverse <- function() x_inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## fn2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$getinverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$setinverse(x_inverse)
    x_inverse
}

# Notes: 
# <<- operator which can be used to assign a value to an object in an environment that is different from the current environment

