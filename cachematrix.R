## Calculates the inverse of a matrix

## Caches the matrix.

makeCacheMatrix <- 
  function(x = matrix()) {

  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m1 <<- solve
  getsolve <- function() m1
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  

}


## Returns the inverse of the matrix from makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m1 <- x$getsolve()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setsolve(m1)
  return(m1)
  
}
