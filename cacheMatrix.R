#The first function, makeVector creates a special "vector", which is really a list containing a function to
#	set the value of the matrix
#	get the value of the matrix
#	set the value of the inverse
#	get the value of the inverse

makeCacheMatrix <- function(mtrx = matrix()) {
	inverse <- NULL		
	set <- function(x) {	
		mtrx <<- x		
		inverse <<- NULL
	}
	get <- function() mtrx
	setinv <- function(inv) inverse <<- inv
	getinv <- function() inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#The following function calculates the inverse of the special matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(mtrx, ...) {
	inverse <- mtrx$getinv()
	if(!is.null(inverse)){
		message("getting cached data")
		return(inverse)
	}
	data <- mtrx$get()
	inverse <- solve(data, ...)
	mtrx$setinv(inverse)
	inverse
}
