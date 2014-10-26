## makeCacheMatrix - wrapper to cache matrix. provides methods to set/get the initial matrix, and cache/retrieve its inverse
## cacheSolve - replacement method for R solve method. takes as argument the output of makeCacheMatrix (instead of regular matrix). caches the inverse matrix.

## wrapper for matric caching. the input is the initial matrix. $set replace the matric (and evicts the cached inverse - if any). $setsolve and $getsolve caches and retrieves the inverse matrix of the current input matrix
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
    
	set <- function(y) {
        x <<- y
        ## evict cache
        s <<- NULL
    }
    
	get <- function() x
    
	setsolve <- function(solved) s <<- solved
    
	getsolve <- function() s
    
	list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## resolves inverse matrix eighter b calculating or resolving the cached result
cacheSolve <- function(x, ...) {
 	## try cache
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
