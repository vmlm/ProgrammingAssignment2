## For the exercise I assume that the matrix supplied is always invertible. 
## That means it is always a square numerical matrix whose determinant != 0.
## Because of this assumption I will not be checking the matrix before using it.

makeCacheMatrix <- function(x = matrix()) {
    ## get/set a matrix and its inverse.
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(get=get, 
         set=set, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        print("Returning cached data:")
        inverse    
    } else{
        inverse <- solve(x$get(), ...)
        x$setinverse(inverse)
        inverse
    }
}
