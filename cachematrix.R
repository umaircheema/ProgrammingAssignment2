## cacheSolve will get the value from cache and makeCacheMatrix function will inversed matrix
## functions do

## This function will add the matrix in cache 

makeCacheMatrix <- function(x = matrix()) {
  invc <- NULL
  set <- function(y) {
    x <<- y
    invc <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invc <<- inverse
  getinverse <- function() invc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return the inverse of the matrix

cacheSolve <- function(x, ...) {
  invc <- x$getinverse()
  if(!is.null(invc)) {
    message("getting cached data")
    return(invc)
  }
  data <- x$get()
  invc <- solve(data, ...)
  x$setinverse(invc)
  invc
}
