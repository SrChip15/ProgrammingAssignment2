## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	
	cache <- NULL ##Store and intializes to null
	
	set <- function(y) {
		x <<- y
		
		cache <<- NULL 
		##Matrix created
	}
	
	get <- function() x ##Gets the value of the matrix
	
	setMatrix <- function(inverse) cache <<- inverse ##Inverses matrix and stores in cache
	
	getInverse <- function() cache ##Gets the inverted matrix from cache
	
	
	list(set = set,
	     get = get,
	     setMatrix = setMatrix,
	     getInverse = getInverse)
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	
	cache <- x$getInverse()
	
	if(!is.null(cache)) {
		message("getting cached data")
		
		return(cache)
		
	}
	
	matrix <- x$get()
	
	tryCatch( {
		
		cache <- solve(matrix, ...)
		
	},
	
	error = function(e) {
		message("Error")
		message(e)
		
		return(NA)
		
	},
	
	warning = function(e) {
		
		message("Warning")
		message(e)
		
		return(NA)
		
	},
	
	finally = {
		
	})
	
	return(cache)
	
        ## Return a matrix that is the inverse of 'x'
}
