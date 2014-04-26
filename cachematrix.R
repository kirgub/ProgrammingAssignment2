## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Matrix Inverse
## get the value of the Matrix Inverse

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


## Calculates the Inverse of the special "Matrix" created
## with the above function. However, it first checks to see
## if the Inverse has already been calculated. If so, it gets
## the Inverse from the cache and skips the computation. Otherwise,
## it calculates the Inverse of the Matrix and sets the value of the 
## Inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
