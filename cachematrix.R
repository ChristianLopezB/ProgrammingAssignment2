## The funtions will calcualte the inverse of a matrix "x" and will retrevie the final answers
##if the function is call again and the matrix "x" still the sam. O/w will recalcualte the inverse of "x". 

## This function creates a special "matrix" object that can cache its inverse by itself

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
  



## This function computes the inverse of the special "matrix" returned by the above function
##If the inverse has already been calculated and the matrix has not changed,
## then this function will retrieve the inverse from the cache to save computational time

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data :D")             ##if "x" is the same it will returns the previoes inv. matrix calcualte
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
