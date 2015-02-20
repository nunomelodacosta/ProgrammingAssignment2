## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix <- function(x = matrix()) {

#}


## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}
makeCacheMatrix <- function(x = matrix()) {
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) ix <<- solve
        getinverse <- function() ix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x = matrix(), ...) {
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting inverted matrix cached data")
                return(ix)
        }
        matrix<-x$get()
        ix<-solve(matrix, ...)
        x$setinverse(ix)
        ix
}