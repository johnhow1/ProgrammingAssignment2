## makeCacheMatrix and cacheSolve takes a matrix, inverses 
## the matrix, then solves the Inverse of the Matrix.

## Function inverses a square matrix.
## q <- matrix(c(4,2,7,6), nrow = 2, ncol = 2)
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
## mat <- makeCacheMatrix(matrix(q,nrow=2))


makeCacheMatrix <- function(x = matrix()){
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function caches the matrix created by makeCacheMatrix
## and solves the matrix. Result should be:
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4

cacheSolve <- function(x, ...){
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
