## Functions to cache result of Matrix inversion since Matrix inversion 
## is usually a costly computation 

## Function to create a special "matrix" object that can be used 
## in caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i  <- NULL
    set  <- function(y){
      x <<- y
      i <<- NULL 
    }
    get  <- function() x
    setinverse  <- function(inverse) i  <<- inverse
    getinverse  <- function() i
    list(set= set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)

}


##  Function to compute the inverse of the special "matrix" 
##  if inverse is not in cache, or  retrieve the inverse f
##  from the cache if inverse result is already in cache for
##  the matrix


cacheSolve <- function(x, ...) {
    i  <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return (i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i

}
