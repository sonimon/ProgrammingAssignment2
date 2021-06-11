## Implements a caching solution to cache the inverse of a matrix
## assumption : the matrix supplied is always invertible

##  steps used for Validating the code 
##  > m1 <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
##  > m1$get()
##  [,1]  [,2]
##  [1,]  1.00 -0.25
##  [2,] -0.25  1.00
##  > cacheSolve(m1)
##  [,1]      [,2]
##  [1,] 1.0666667 0.2666667
##  [2,] 0.2666667 1.0666667
##  > m1$get() %*% cacheSolve(m1)
##  getting cached data
##  [,1] [,2]
##  [1,]    1    0
##  [2,]    0    1

## The cacheSolve(m1)  call in the last step used the cached value
## multiplying matrix with inverse returns the identity matrix
##



## makeCacheMatrix creates a special "Matrix", 
## which is a list containing a function to do the following

## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse Matrix
## get the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
     invmatrix <- NULL
     set <- function(y) {
          x <<- y
          invmatrix <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) invmatrix <<- inverse
     getinverse <- function() invmatrix
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     invmatrix <- x$getinverse()
     if(!is.null(invmatrix)) {
          message("getting cached data")
          return(invmatrix)
     }
     data <- x$get()
     invmatrix <- solve(data, ...)
     x$setinverse(invmatrix)
     invmatrix
}
