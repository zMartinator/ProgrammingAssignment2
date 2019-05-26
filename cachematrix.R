## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Description:
## Makes a matrix object with get and set functions that allow a cache of the inverse to be stored
## note: every time the matrix is set() the inverse cache is set to NULL
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Description:
## Returns the cached inverse if it's already been calculated. If not then it calculates the inverse, stores it in the cache and then returns the value. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
