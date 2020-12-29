## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Returns a list of functions enabling the storing of data (will be used to stored matrices and their inverses)
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
## Function takes a cache matrix. Should the matrices inverse be calculated already, it returns it.
## Otherwise, it will calculate the inverse, store it and resturn it
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

testMatrix <- matrix(1:4, 2, 2)
testFunction <- makeCacheMatrix(testMatrix)
cacheSolve(testFunction)
cacheSolve(testFunction)
