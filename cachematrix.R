"For the exercise I assume that the matrix supplied is always invertible.
 That means it is always a square numerical matrix whose determinant != 0.
 Because of this assumption, I will not be checking the matrix before using it."

## get/set a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    "The function returns a list representing a CacheMatrix structure. The 
     variables x (the matrix) and inverse (the matrix inverse) are defined, but 
     aren't included in the returned list. This is because they're meant to be 
     modified through the get/set functions, not direct manipulation."
    
    inverse <- NULL #inverse starts off as NULL
    
    # defining the matrix get and set functions
    get <- function() x # simply returns the matrix
    set <- function(y) {
        "Changes the value of x to the given matrix y. 
         Also reverts the value of inverse to NULL. The inverse will have to be 
         recalculated after executing the set function."
        x <<- y
        inverse <<- NULL
    }
    
    #defining the inverse get and set functions
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    
    "NOTE: It's important to note that the get/set functions are taking
     advantage of R's lexical scoping. In other words, the get/set 
     functions can access the inverse and x variables because R considers their
     DEFINING environment (in this case the makeCacheMatrix function definition)
     rather than their calling environment (the global environment, if the 
     function is called from the console) when searching for the associated 
     values."
    
    #return a CacheMatrix
    list(get=get, 
         set=set, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

## Return a matrix that is the inverse of  the 'x' CacheMatrix
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() #get the value of inverse
    if (is.null(inverse)) {
        "If the value DOESN'T exist, calculate it using the solve function.
         This implies that if the value exists, the solve function will not be 
         called unnecessarily."
        inverse <- solve(x$get(), ...) #calculate the value...
        x$setinverse(inverse) #... and cache it in the CacheMatrix.
    }
    inverse #return inverse
}
