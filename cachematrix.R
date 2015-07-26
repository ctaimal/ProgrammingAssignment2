# Cache a matrix to calculate the inverse of it

makeCacheMatrix <- function(M = matrix()) {
    
    # Initializing the inverse of the matrix.
    i <- NULL
    
    # Setting the matrix
    set <- function(y){
        M <<- y
        i <<- NULL
    }
    
    # Getting the matrix
    get <- function() M # Returning the matrix
    
    # Setting the inverse
    setinv <- function(solve) i <<- solve
    
    # Getting the inverse
    getinv <- function() i # Returning the inverse
    
    # Return all the methods listed above in a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# cacheSolve, will calculate the inverse of the special "matrix" returned
# by the "makeCacheMatrix" object. If the inverse of the matrix has been
# computed, cacheSolve will print it, otherwise it will proceed to calculate it.

cacheSolve <- function(x, ...) {
        
        # Getting the inverse from the 'makeCacheMatrix' object
        # If the inverse matrix has been calculated it will be returned
        M <- x$getinv()
        
        # Otherwise,  if the inverse has not been calculated, print this
        if( !is.null(M) ) {
            message("getting cached data")
            return(M)
        }
        
        ###################################
        # Compute the inverse of the matrix
        ###################################
        
        # Get the matrix from the 'makeCacheMatrix' object
        data <- x$get()
        
        # Compute the inverse of the matrix
        M <- solve(data)
        
        # set the inverse of the matrix in the object
        x$setinv(M)
        
        # Return the inverse of matrix
        M
}
