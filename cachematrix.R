## The two functions, when used together, create a cache of a matrix
## and then perform the inverse on that cache. This is beneficial in situations
## in which a matrix is so large that it is computationally exhaustive to 
## consistently recalculate the inverse. 

## To test the functions, create an n-by-n square matrix
## Ex: testmatrix <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
## then cache the matrix using the makeCacheMatrix function
## Ex: cachematrix <- makeCacheMatrix(testmatrix)
## Finally, run cacheSolve on the cached matrix
## Ex: cacheSolve(cachematrix)
## You can run cacheSolve one more time to confirm that it will return the 
## previously computed data.

## The first function, makeCacheMatrix, creates a list 
## containing a function that
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse
## It uses the solve function, which returns the inverse
## of a matrix A where A is a square matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<-- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse on the cache matrix. 
## If the cached inverse already exists, it will return 
## the data rather than recomputing the inverse. 

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
