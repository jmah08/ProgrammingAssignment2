## Two functions that will allow user to calculate, cache and retrieve the inverse of a
## square, non-singular matrix.

	## Set of functions to handle caching and getting the inverse of an input matrix. 
	## Resets cache and matrix on each subsequent call.

	makeCacheMatrix <- function(x = matrix()) {
		I <- NULL

		## Show warning message if input matrix is not square or is singular - either
		## case will cause an error in the solve() function.
		testMatrix <- function(mtrx) {
			if(nrow(mtrx) != ncol(mtrx)) 
				message("Warning: Matrix is not square. cacheSolve won't work!!")
			else if(det(mtrx)==0) 
				message("Warning: Matrix is singular. cacheSolve won't work!!")
		}
		
		testMatrix(x)

		set <- function(y) {
	    		testMatrix(y)
          		x <<- y
          		I <<- NULL
      	}
     
     		get <- function() x
     		setInverse <- function(Inverse) I <<- Inverse
     		getInverse <- function() I

     		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}


	## Checks cache for inverse of input matrix. If found, returns cached value. If not found,
	## calculates the matrix inverse, sets the value in cache, then returns the inverse. 
	## Leverages makeCacheMatrix().

	cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
     		I <- x$getInverse()
     
     		if(!is.null(I)) {
          		message("getting cached data")
          		return(I)
     		}
     
     		data <- x$get()
          	I <- solve(data, ...)
     		x$setInverse(I)
     
     		I
	}
