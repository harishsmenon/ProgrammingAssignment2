## A pair of functions that cache the inverse of a matrix. First, supply a 
## an invertible matrix to makeCacheMatris function. Second, compute or 
## retrieve from cache the matrix inverse using cacheSolve function.

## This function creates a special "matrix" object that can cache its inverse.
## This function assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the matrix inverse object
        minv <- NULL
        
        ## set function to set the matrix object
        set <- function(y) {
                x <<- y
        }
        
        ## gets function to retrieve the matrix object
        get <- function() x
        
        ## set function to set the matrix inverse
        setInv <- function(z) minv <<- z
        
        ## get function to retrieve the matrix inverse
        getInv <- function() minv
        
        ## list object containing the set, get, setInv and getInv functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


##  This function computes the inverse of the special "matrix", returned by 
## makeCacheMatrix above, using the "solve" function in R . If the inverse has 
## already been calculated (and the matrix has not changed), then the function 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## retrieves the matrix inverse from cache
        minv <- x$getInv()
        
        ## returns the matrix inverse if avaiable in cache
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        ## if matrix inverse is not available in cache, then retrieve matrix
        ## from cache
        data <- x$get()
        
        ## compute matrix inverse
        minv <- solve(data, ...)
        
        ## store the matrix inverse in cache
        x$setInv(minv)
        
        ## returns matrix inverse
        minv
}
