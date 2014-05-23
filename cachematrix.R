## Put comments here that give an overall description of what your
## functions do
## These two functions are designed to speed the computation of matrix inversion
## This is done by caching the the computed inversion rather than computing it repeatedly

## Write a short comment describing this function
## Creates a special matrix object that can cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setSolve <- function(Solve) m <<- Solve
	getSolve <- function() m
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}


## Write a short comment describing this function
## Computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- Solve(data, ...)
	x$setSolve(m)
}
