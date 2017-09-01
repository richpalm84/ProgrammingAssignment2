## The functions below do two things. The first function
## puts an empty matrix, or one just created, into the cache.
## The second function finds the inverse of a matrix it is given by
## pulling the answer from the cache, if available.

## This function stores the inverse of a matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the data is in the cache, and
## then pulls it from the cache if available.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
