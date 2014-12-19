## Coursera R Programming Assignment 2 - Lexial Scoping. Write an R functio
## that is able to cache potentially time consuming computations. Here, we 
## cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" that stores a 
## matrix and cache its inverse. This object is really a list containing a 
## function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set, 
		get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. However, it first checks to see 
## if the inverse has already been computed or not. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## mean of the data and sets the value of the mean in the cache via the 
## setInverse function   

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
    	message("getting cached data")
    	return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
