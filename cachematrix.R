## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          im <- NULL 
          set <- function(y){
          x <<- y 
          im <<- NULL
        }
          get <- function() x 
          setIM <- function(inverse) im <<- inverse
          getIM <- function() im
          list(set = set, get = get, setIM = setIM, getIM = getIM)
}


## This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getIM() 
        if (!is.null(im)){
          message("getting cached inverted matrix")
          return(im)
        }
        mat <- x$get()
        im <- solve(mat, ...)
        x$setIM(im) 
        im
}
