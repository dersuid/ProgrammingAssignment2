

## the first function calls a squared matrix, store and solve it (get the inverse) for the first time
## the second function, calls the first function in order to confirm if that matrix
## has already its inverse, if not, compute the inverse for x matrix

## this function has for functions within it. First save the x matrix, get the inverse of it, 
## keep the solution and store in in cache, and then returns a list containing the 4   functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## this function, validates if the inverse of x has been already computed, if not, then
## computes the inverse for the first time, if it is already estimated, then calls the makeCacheMatrix
## and take the result of solve (x) for returning

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
