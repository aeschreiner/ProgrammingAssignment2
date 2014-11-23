## Pair of functions that create a list structure to enable caching of a
##  computed matrix inverse

## Create a list object that stores and accesses an inverse to the supplied
##  matrix

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinv <- function(inv) mi <<- inv
    getinv <- function() mi
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## return the matrix inverse from the given matrix structure
##  return the cached value of the inverse if found, otherwise compute
##  the inverse and return it

cacheSolve <- function(x, ...) {
    # check if the inverse matrix is cached, if so return it
    mi <- x$getinv()
    if (!is.null(mi)) {
        message('getting cached inverse')
        return(mi)
    }
    # compute the matrix inverse, cache it, then return it
    data <- x$get()
    mi <- solve(data, ...)
    x$setinv(mi)
    mi
}
