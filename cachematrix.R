## The following 2 functions will help us to cache the inverse of a matrix. 
## Calculating the inverse of a fairly large matrix is a resource intensive process, 
##   hence if we are going to need the inverse of the same matrix over and over again in the same session, its advised to cache it.

## The following function creates an interface to a matrix.
## Returns a list consisting of functions which help to set & get the matrix and its Inverse.
makeCacheMatrix <- function(x = matrix()) {
	m_inv <- NULL
	set <- function(y)
	{
		x <<- y
		m_inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inv, ...) m_inv <<- inv
	getInverse <- function() m_inv
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## CacheSolve function checks if the Inverse has already been calculated for the underlying matrix, if not, calculates the Inverse and caches it for future reference.
##  Further calls to this function with the same argument(matrix interface created using above makeMatrix function) simply returns the cached value of the Inverse. 
cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv))
	{
		message("returning cached Inverse")
		return (inv)
	}
	temp = x$get()
	inv <- solve(temp)
	x$setInverse(inv)
	inv
}