## The purpose of these two functions together is to create a way to store 
## or cache the inverse of a matrix produced by the solve() function. 


## The function makeCacheMatrix (below) takes a matrix as an argument and 
## returns a list with four functions. This list of four functions serve 
## to cache the matrix the makeCacheMatrix function takes as an argument 
## in an object in an environment outside that defined by the function.

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


## The function cacheSolve (below) searches to see if the inverse of the
##  matrix has already been calculated and stored somewhere in the R 
## environment. If so, it returns the inverse stored in the object 
## it finds. If it cannot find the inverse, the function will calculate 
## the inverse itself.

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


