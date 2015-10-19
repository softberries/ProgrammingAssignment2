## 'makeCacheMatrix' creates a special "vector", which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set the matrix value
  set <- function(matrixToStore) {
    x <<- matrixToStore
    inverse <<- NULL
  }
  # get the matrix value
  get <- function() x
  # set the calculated matrix inverse
  setinverse <- function(inv) inverse <<- inv
  # get calculated matrix inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' function returns a matrix that is the inverse of 'x'
## if the matrix inverse is not found (first call) it calculates the inverse
## for the given matrix 'x', stores the result in cache and returns it. 
## Every subsequent call will return already calculated inverse stored in cache
cacheSolve <- function(x = matrix(), ...) {
  ## check if the inverse is available and return it if it already exists
  matrixInverse <- x$getinverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  ## if the inverse was not found in cache calculate it, store for later calls and return
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setinverse(matrixInverse)
  matrixInverse
}
