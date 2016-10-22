## makeCacheMatrix accepts a matrix as input and caches it in memory for quick retrieval
## cacheSolve accepts a matrix cache function and attempts to return the cached or computed inverse of the matrix 
## Examples:
## 10x10 Matrix initialized randomly without repeating values
## mtrx <- matrix(sample(c(1:100), size = 100, replace = TRUE), nrow=10, ncol=10)
## mtrxobject <- makeCacheMatrix(mtrx)
## cacheSolve(mtrxobject)
## mtrxobject$get() #retrieves the original matrix
## mtrxobject$getmatrix() #retrieves the cached inverse of the matrix
##
## 4x4 matrix initialized randomly without repeating values, setting a new 4x4 matrix to be cached
## mtrx2 <- matrix(sample(c(1:16),16,replace=TRUE), nrow=4, ncol=4)
## mtrxobject$set(mtrx2)  #sets the NEW non-inverse matrix
## mtrxobject$get()       #retrieves the non-inverse matrix
## mtrxobject$getmatrix() #Will return a null because a new matrix was set and the inverse has not been cached
## cacheSolve(mtrxobject) #Inverse matrix is now calculated and cached
## mtrxobject$getmatrix() #Will return the inverse matrix now i.e. the cached inverse is no longer NULL

## Function that contains a list of functions to get and set a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(inmatrix) cm <<- inmatrix
  getmatrix <- function() cm
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Function that retrieves a cached inverse matrix (if it exists) or caches of the inverse matrix (if it does not exist)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cm <- x$getmatrix()
  
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  
  data <- x$get()
  
  cm <- solve(data, ...)
  x$setmatrix(cm)
  cm
}
