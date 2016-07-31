## The code showed here caches the inverse of a matrix.

## The function makeCacheMatrix creates a special -matrix- wich lists a function to 1. set the value of the matrix. 2. get the value of the matrix. 3. set the inverse of the matrix. 4. get a matrix with the inverse of the original matrix. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve solves the inverse of the original matrix in the fuction makeCacheMatrix. It is coded to first check if the inverse has already been solved (if it has, it gets the inverse from the cache and skips the computation). If it hasn't been solved, it solves the inverse of the matrix and sets the solution of the inverse in the cache by the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
