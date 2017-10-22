## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object that can cache
## its inverse. It returns a list of functions:
## set: cache the matrix
## get: returns the matrix
## setInv: cache the inverse of the matrix from input
## getInv: returns the inverse from cache
makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y){
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) xinv <<- inverse
	getInv <- function() xinv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated and 
## the matrix has not changed, the cacheSolve will retrieve the inverse 
## from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getInv()
        data <- x$get()
        if(!is.null(xinv)){
        	message('inverse has already been cached')
        	xinv
        }
        
        xinv <- solve(data)
        x$setInv(xinv)
        xinv
}
