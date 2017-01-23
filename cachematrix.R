## Inverting a matrix (via solve()) often takes up alot of computational resources.
##  these function check to see if a cached value of the inverted matrix is available
##  prior to computing, rather than executing the solve() function, in order to save 
##  computational time & resrouces.

## Creates a special matrix, which is really a list containing a funciton to 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. solve the matrix
##  4. get the solved matrix

makeCacheMatrix <- function(x = matrix()) {

    invrs <- NULL
    ## initialize the set function
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    
    ##initilize the get function
    get <- function() x
    
    ##sets the inverted matrix
    setinverse <- function(inverse) invrs <<- inverse
    
    #gets the inverted matrix
    getinverse <- function() invrs
    
    ## create list of contained functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves a matrix (gets it's inverse), but first checks to see if matrix has already been solved.
##    If so, it gets the inverted ,atrix from the cache and skips the initialization. Otherwise
##    it inverts data via the setinverse funciton.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invrs<- x$getinverse()
    
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
    
}
