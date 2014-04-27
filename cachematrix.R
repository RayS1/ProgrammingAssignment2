## Two functions makeCacheMatrix and cacheSolve together determines inverse
## of a matrix in an efficient manner, by chching earlier computed results.

## Function makeCacheMatrix creates an object x for which it exposes 
## methods set, get, setInverse, and getInverse. 

makeCacheMatrix <- function(x = matrix()) {
    my_inverse <- NULL
    set <- function(y) {
        x <<- y
        my_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve_result) my_inverse <<- solve_result
    getInverse <- function() my_inverse
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function cacheSolve returns inverse of the matrix x. If the inverse
## has been already calculated in a prior call to makeCacheMatrix then 
## the result (i.e., inverse matrix) is fetched from cache. Otherwise
## inverse is calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    my_inverse <- x$getInverse()
    if(!is.null(my_inverse)) {
        message("getting cached data")
        return(my_inverse)
    }
    data <- x$get()
    my_inverse <- solve(data, ...)
    x$setInverse(my_inverse)
    my_inverse
}