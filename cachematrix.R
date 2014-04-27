## The below functions create an inverse of a matrix and cache it.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## create an inverse-able matrix
makeCacheMatrix <- function(x = matrix()) {

  value <- NULL
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) value <<- solve
  getinverse <- function() value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## if the matrix is already inversed, return the inverse from cache, 
## else, inverse and save the matrix in cache and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  value <- x$getinverse()
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setinverse(value)
  value
}  