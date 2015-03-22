## The first function takes the matrix and creates a list of different
## functions that can handle that matrix.
## The second function takes the list created in the first function as its
## argument and looks for a cached inverse to give, otherwise it computes
## the inverse, caches it, and prints it.

## makeCacheMatrix is a funtion that takes one argument x, and returns a
## list of functions you can call

makeCacheMatrix <- function(x = matrix()) {
    ## if you're invoking makeCacheMatrix you are giving it a new matrix
    ## so you start by setting the inverse as null
    inv <- NULL
    
    ## defining the four functions inside the makeCacheMatrix function
    set <- function(y) { 
        x <<- y
        m <<- NULL ## will define the inverse and the x in the 
        ## makeCacheMatrixenvironment
    }
    
    get <- function() x ## just retrieves x
    
    setinverse <- function(inverse) inv <<- inverse ## setting the inverse
    ## in the MakeCacheMatrix environment therefore overwriting it's previous
    ## NULL value
    
    getinverse <- function() inv ## just retrieves the inverse
    
    ## returning the list
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}



## Takes as argument the list returned in the previous function
## Returns the inverse of the matrix either from the cache or by 
## calculating it

cacheSolve <- function(x, ...) {
        
        ## looks for an inverse already there and cached and prints
        ## it if it finds it
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        ## if inverse not already there it gets the data from your 
        ## previous call and save of output of makeCacheMatrix, 
        ## calculates the inverse, puts it in the list, and prints 
        ## the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
