## Cache and get the inverse of the matrix

## This function creates a cache for the inverse of the matrix as the argument

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinvr <- function(inverse) invr <<- inverse
  getinvr <- function() invr
  list(set = set, get = get,
       setinvr = setinvr,
       getinvr = getinvr)
}


## Calculate the inverse of the matrix above returns
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  invr
}
