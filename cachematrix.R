## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix object which we can cache the inverse for
##
## Four functions: set, get, setinverse, getinverse
##
##
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## Operate on our special matrix object to calculate the inverse or use the cached result.
## If the inverse exists get the cached data otherwise use the solve() to calculate inverse
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## either after checking if inverse is cached
        ## by using return(m)
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached data")
              return(m)
        }
        ## or by solving the inverse
        ## setting the inverse 
        ## and then returning it, 
        ## we do not need a return() since it is the last statement
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
  
}
