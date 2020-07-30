## Put comments here that give an overall description of what your
## functions do
## The function calculates the result of the inverse of a matrix
## with caching enabled. If the inverse has previously been calculated,
## it would be stored already in the cache matrix object, and 
## therefore can be returned directly with calculation

## Write a short comment describing this function
## Creates an object containing 4 functions
##  1. set the matrix (program never used)
##  2. get the matrix 
##  3. set the inverse of the matrix
##  4. get the inverse of the matrx
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## The function first tries to get the inverse from the cache matrix object
## if it exists, then return as-is
## if it doesn't, calculate the inverse using solve()
## store the result in the cache matrix object
## then return the result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
