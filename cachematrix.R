

# returns a list of four functions
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# returns an inverse of the original input matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #gets the inverse from a cache...
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return
  }
  
  # ...or calculates the inverse
  data <- x$get()
  m <- solve(data, ...)
  
  # sets the value of the inverse in the cache
  x$setInv(m)
  return(m)
  
}
