## Making the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##setting initial inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##function to retrieve the matrix x
  setinv <- function(inverse) inv <<- inverse  ##function to retrieve inverse of matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Obtaining cached data
cacheSolve <- function(x, ...) {    
  inv <- x$getinv()
  if(!is.null(inv)) {       ## Checking if inverse is NULL
    message("getting cached data")
    return(inv)             ## Return inverse of matrix
  }
  data <- x$get()
  inv <- solve(data, ...)   ## Calculates inverse of matrix
  x$setinv(inv)
  inv
}