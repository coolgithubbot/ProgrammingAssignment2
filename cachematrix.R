##The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
        x <<- y
        m <<- NULL
        }
get <- function() x
setSolve <- function(solve) m <<- solve
getSolve <- function() m
list(set = set, get = get, 
        setSolve = setSolve,
	getSolve = getSolve)
}
##This function computes the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has already been calculated 
##(and the matrix has not changed)
##then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
   m <- x$getSolve()
   if(!is.null(m)) {
        message("getting cached data")
	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m  ## Return a matrix that is the inverse of 'x'
}
