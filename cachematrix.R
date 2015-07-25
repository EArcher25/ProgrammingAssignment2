## makeCacheMatrix is a function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}


## cacheSolve is a function that computes the inverse of the special matrix created by makeCacheMatrix 
## however if the inverse has already been created (and not changed) then the function retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}