## The CacheSolve function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. CacheSolve function  
## first checks to see if the inverse has already been calculated and stored in the cache. 
## If it finds it, it skips the calculations otherwise
## it calculates the inverse of the matrix and sets the value of the inverse in the cache

## The following function returns a list of basic action functions on the Matrix 
## Set, Get, Set inverse and Get inverse
makeCacheMatrix <- function(x = matrix()) {

# initialize inverser variable to NULL
    inv <- NULL

    # Another function to set the value of the matrix
    set <- function(y) {
        x <<- y   # This is the Global Assignment 
        inv <<- NULL # Set the Inv back to Null at global level
    }
    # Get the value of the matrix
    get <- function() x
    # Set the inverse
    setinv <- function(inv_) inv <<- inv_
    # to get the inverse
    getinv <- function() inv

    # list of all the functions withing this function is returned
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    


}


## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. 
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv <- x$getinv() # check if the inverse is already cached
    if(!is.null(inv)) {   # If It is cached then simply return inv
        message("getting cached data")
        return(inv)
    }
    # If not cached, then set the global value (cache with the inverse)
    data <- x$get()
    # Compute the inverse
    inv <- solve(data, ...)
    # Cache the inverse and return the inverse just calculated
    x$setinv(inv)
    
    inv
        
}
