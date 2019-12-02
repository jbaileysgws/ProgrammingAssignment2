## This will create a matrix object to cache its inverse

makeCacheMatrix <- function (x=matrix() ) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function () inv
  list (set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

##This function will retrieve the inverse returned by the above function

CacheSolve <- function (x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
}