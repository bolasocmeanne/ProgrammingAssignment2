makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function(){inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
##We're asked to write two R functions (makeCacheMatrix and cacheSolve) that can cache potentially time-consuming computations. These computations could relate to a very long## 
##vector of which we require the mean or a matrix of which we want to calculate the inverse. Making use of scoping rules is one of those special cases when we'd not want to go ##
##through the same computational process multiple times and when the contents of our vector or matrix are not changing.##

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##Caching the value of the mean of a vector or the inverse of a matrix would mean that they'd be called directly from the cache when required without any computation.##