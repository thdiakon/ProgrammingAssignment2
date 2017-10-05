## These two functions are used together in order to compute and cache 
## the computed result of an Inverse matrix.

## makeCacheMatrix creates a matrix object, that can cache its inverse.
## (Always in case the matrix is invertible)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        
    }
    get <- function() x
    setinverse <-function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
##  In case the matrix is already calculated, it retrieves the result from cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
			inverse <- x$getinverse()
	
    if(!is.null(inverse)){
        message("getting cached inverse matrix ")
        return(inverse)
    }
    
    data <-x$get()
    inverse <-solve(data, ...)
    x$setinverse(inverse)
    inverse
}

## Testing the functions after sourcing them...

#> m <- matrix(rnorm(12),3,3)
#> m_test=makeCacheMatrix(m)
#> cacheSolve(m_test)
#           [,1]        [,2]       [,3]
#[1,] -0.2380563  0.06940297  1.9958600
#[2,]  0.5542556  0.08918635 -0.7654154
#[3,] -0.2411843 -0.45187542  2.2984332
# Ask to compute again: (retrieved from cache)
#> cacheSolve(m_test)
#getting cached matrix data
#           [,1]        [,2]       [,3]
#[1,] -0.2380563  0.06940297  1.9958600
#[2,]  0.5542556  0.08918635 -0.7654154
#[3,] -0.2411843 -0.45187542  2.2984332
