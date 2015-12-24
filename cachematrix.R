## Couple of functions to optimize the use of the inverse of a matrix. 
## For a given matrix, the inverse is created the first time it is requested. 
## The inverse is then stored in a cache associated with the source matrix. 
## Subsequent requests to the inverse retrieve its value from the cache. 

## makeCacheMatrix will create the cache environment for the source matrix 
## with a list of sub-functions. 
## cacheSolve returns the inverse of the matrix by using the environment 
## created by the previous function. If not already present in the cache, 
## the inverse is created with solve() and then stored in the cache. 

## Usage: 
## 1) 'xPlus <- makeCacheMatrix(x)' to produce the list of functions needed 
## for matrix 'x'. 
## 2) 'cacheSolve(xPlus)' to return the inverse from matrix 'x'.  


## makeCacheMatrix - Generates a list of functions to set/get a matrix 
## and its inverse in/from the cache

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        i <<- inv
    }
    getinv <- function() {
        i
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - Returns the inverse of an input matrix, either from 
## the cache if already there, or from a call to the solve() function, 
## in which case the generated inverse matrix is stored in the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # 1/ Lookup the matrix inverse in the cache 
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # 2/ If no cached inverse, create it 
    # No test done on the input matrix, as we assume it is invertible 
    m <- x$get()
    i <- solve(m) 
    x$setinv(i) # caches the inverse 
    i

}
