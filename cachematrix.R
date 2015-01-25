## These funcitons are to compute an inverse of matrix. If the inverse is
##calculated before, the cache is used without repeating calculation.

## The first function creates a special matrix that can cache its inverse.
## It is a list which consists of 3 functions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list( get = get, 
          setinv = setinv,
          getinv = getinv)
}

## The second funciton computes the inverse of the special matrix created 
## by the previous function. If the inverse is already calculated,
## the function will call the inverse from cache. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cache data")
      return(inv)  ## Return from cache
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv  ## Return a matrix that is the inverse of 'x'
}
