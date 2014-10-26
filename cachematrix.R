# This R script provides two functions, `makeCacheMatrix` and `cacheSolve`
# which provide functionality for caching the results of calls to the
# `solve` function (computing the inverse of a matrix).
#
# First, construct a special list which acts as a 'cacheable matrix'
# object, then call the function `cacheSolve` in the same way the function
# `solve` would be called. If the inverse has already been computed, it
# is not recomputed, e.g.:
#
#   > mymatrix = makeCacheMatrix(matrix(c(2,2,3,2), 2, 2))
#   > cacheSolve(mymatrix)
#        [,1] [,2]
#   [1,]   -1  1.5
#   [2,]    1 -1.0
#   > cacheSolve(mymatrix)
#   getting cached data
#        [,1] [,2]
#   [1,]   -1  1.5
#   [2,]    1 -1.0

# Create a list which acts as a 'cacheable matrix'.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
	inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Like the function `solve`, but used with a list created by
# `makeCacheMatrix`. Only computes the inverse if not already cached.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
	return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
