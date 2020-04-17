## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
	invrse = NULL
	
	## Method to set the matrix
	set <- function(y){
		x <<- y
    	invrse <<- NULL
	}
	
	## Method to get the matrix
	get <- function() x
	
	## Method to set the inverse
	setinvrse <- function(i) invrse <<- i
	
	## Method to get the inverse
	getinvrse <- function() invrse
	
	## Return list
	list(set = set,
	     get = get,
	     setinvrse = setinvrse,
	     getinvrse = getinvrse)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix()
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	invrse <- x$getinvrse()
	
	## Return the inverse if it's already there
	if (!is.null(invrse)){
		message("getting cached data")
		return(invrse)
	}
	
	## Otherwise compute the inverse and cache the inverse
	message("compiling inverse")
	data <- x$get()
	invrse <- solve(data, ...)
	
	## Set the inverse
	x$setinvrse(invrse)
	
	## Return the inverse
	invrse
}
