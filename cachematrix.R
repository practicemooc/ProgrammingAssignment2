makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x;
  setMinv <- function(minv) m <<- minv;
  getMinv <- function() m;
  list(set = set, get = get, setMinv = setMinv, getMinv = getMinv)
}


cacheSolve <- function(x, ...) {
  m <- x$getMinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMinv(m)
  m
}
