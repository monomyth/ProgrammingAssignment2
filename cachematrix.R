## makeCacheMatrix creates a new matrix object that can store its own inverse
## cacheSolve returnes cached inverse of matrix produced by makeCacheMatrix or
## calculates and caches it if the inverse wasn't calculated before


## create a special matrix that can store cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y,...) {
    x <<- matrix(y,...)
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## try to retrieve cached inverse matrix,
## if not avaliable, then calculate, cache and return
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
