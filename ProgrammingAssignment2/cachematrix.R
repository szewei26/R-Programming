## This function creates a special matrix that can cache its inverse
## It then computes the inverse of the matrix
## If the inverse has already been calculated, cacheSolve should retrieve the inverse from the cache

## This function creates a special matrix object that can cache its inverse
## It creates a list containing the functions:
## set: set the values for the matrix
## get: get the values in the matrix
## setinverse: solve for the inverse
## getinverse: get the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated, then it will retrieve from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## Sample of a test run
## > test<-makeCacheMatrix(matrix(c(8,4,3,2),2,2))
## > test$get()
## [,1] [,2]
## [1,]    8    3
## [2,]    4    2
## > cacheSolve(test)
## [,1]  [,2]
## [1,]  0.5 -0.75
## [2,] -1.0  2.00
## > cacheSolve(test)
## getting cached data
## [,1]  [,2]
## [1,]  0.5 -0.75
## [2,] -1.0  2.00
