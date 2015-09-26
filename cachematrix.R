## This is the second programming assignment for the R Programming class.
## These 2 functions will create a new matrix object type that can cache
## the inverse of it's matrix so that the calculation does not need to be 
## run each time the inverse is required in order to save time/computational
## power.

## makeCacheMatrix:  This function creates a new matrix object that stores 
## the inputted matrix and a cached value of the inverse if using cacheSolve
## to calculate the inverse.  Access to them matrix and inverse is by using the 
## get and getinverse functions.  Values can be set by using set and setinverse
## but directly accessing is unnecessary since makeCacheMatrix and cacheSole
## handle that.  Note that when setting the matrix the cache value will be null
## until cacheSolve is used on the object to solve for the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(m_inverse) i <<- m_inverse
  getinverse <- function() i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function will solve for the inverse of the matrix contained
## within the returned matrix object from makeCacheMatrix function.  If checks
## to see if the inverse is available first and if so returns that value.  If 
## not already calcuated then it will solve, set it in cache, and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("solving for inverse")
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
