input
## makeCacheMatrix creates a special "vector", which is really a list containing a function 
## to: 
## set --> where the matrix is being stored in x 
## get --> where x is being returned
## getsolve --> get the inversed matrix
## setsolve --> set the inversed matrix

makeCacheMatrix <- function(x = matrix()) {

  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(Invmat) m <<-Invmat
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the inveresed matrix by using the special "vector" created with the 
## above function. it first checks to see if the inversed matrix has already been calculated. 
## If so, it gets the inveresed matrix from the cache and skips the computation. 
## Otherwise, it calculates the reverse matrix of the data and sets the matrix in the cache
## via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
