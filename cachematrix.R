## These are a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) t <<- inverse
        getinverse <- function() t
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        freshdata <- x$get()
        s <- solve(freshdata, ...)
        x$setinverse(s)
        s
}