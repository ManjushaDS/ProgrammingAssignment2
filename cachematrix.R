## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getInverse()
  # if the inverse has already been calculated
  if (!is.null(inv)) {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(inv)
  inv
}