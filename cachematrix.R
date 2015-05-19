## These functions allow users to pull from a cached list of inverses for a matrix
## If the inverse of the matrix you input has already been calculated it will pull it from the cache
## If not it will compute the inverse

## This creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse or grabs from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
