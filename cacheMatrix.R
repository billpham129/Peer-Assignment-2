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