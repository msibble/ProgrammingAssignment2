## Making cache the inverse of matrix 
## Already assumed that said matrix is already invertible

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
			inv <- NULL 
			set < - function(y){
			x<<- y 
			inv <<- NULL 
		}
		get <- function () x 
		setsolve <- function (solve) inv <<- inv
		getsolve <- function () inv
		list( set= set, 
			get = get, 
			setsolve = setsolve
			getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makecachematrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
