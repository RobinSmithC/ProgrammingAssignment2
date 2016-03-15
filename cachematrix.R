## Program originally forked and is part of an assignment in R Programming and part of Data Science offering by John Hopkins on Coursera.
## The script contains two functions, one creates a list of 4 functions representing a matrix and its inverse.
## A special matrix.
## The second function uses the special matrix and calculates its inverse and stores said. If the inverse was already calculated, time is saved by not recomputing
## the inverse but instead retrieve the value already stored. A tradeoff of memory versus CPU usage.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	
	## stores cached inverse matrix
	inverseMatrixCached <- NULL

	## stores the matrix object specified and erases the cached object
      set <- function(y) {
            x <<- y
		inverseMatrixCached <<- NULL
      }
      
	## returns the matrix object specified and an empty matrix is none is specified
	get <- function() x

	## stores the inverse which is a matrix representing the inverse of a matrix. The inverse is said to have been cached
	setinverse <- function(inverseMatrix) inverseMatrixCached  <<- inverseMatrix

 	## return the cached matrix
	getinverse <- function() inverseMatrixCached 

	## return the special matrix which is a list of functions defined above
	list(set = set, get = get,
             getinverse = getinverse,
             setinverse = setinverse )
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)

	## keep a copy of the inversed matrix
	x$setinverse(m)

	## return the inversed matrix
	m
}
