## makeCacheMatrix and cacheSolve functions create a matrix and its inverse.
## To avoid recurring costly computations, we use the <<- operator, to 
## cache the result, i.e. the computation is done only once for a defined matrix
## The "matrix" manipulations are done by creating an object
## , i.e. a list, containg the matrix, its inverse and the functions 
## to set and get their values respectively


## makeCacheMatrix function creates and returns a "matrix" object.
##
## "matrix" object definition:
## x: matrix
## x functions: set: to set "x" value, get: to get "x" value 
## ix:  matrix inverse
## ix functions: setinverse: to set "ix" value, getinverse: get "ix" value
##
## Args:
##      x: matrix object, default value 1x1 matrix with NA
## Returns:
##      list as per "matrix" object definition

makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        
        # Caches matrix inverse result
        setinverse <- function(solve) ix <<- solve
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the "matrix" returned 
## by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## Args:
##      x: matrix object, and ... special variable length argument
## Returns:
##      ix: the inverse of matrix x
cacheSolve <- function(x = matrix(), ...) {
        ix <- x$getinverse()
        
        # test if inverse was calculated before and the matrix has not changed
        if(!is.null(ix)) {
                message("getting inverted matrix cached data")
                return(ix)
        }
        matrix<-x$get()
        ix<-solve(matrix, ...)
        # Caches matrix inverse result with setinverse function of "matrix" object
        x$setinverse(ix)
        ix
}
