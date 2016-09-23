makeCacheMatrix<- function(x = matrix()) {
  var_a <- NULL
  set <- function(y) {
    x <<- y
    var_a <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) var_a <<- solve
  getinverse <- function() var_a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  var_a <- x$getinverse()
  if(!is.null(var_a)) {
    return(var_a)
  }
  data <- x$get()
  var_a <- solve(data, ...)
  x$setinverse(var_a)
  var_a
}
