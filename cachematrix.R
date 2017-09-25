## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  ##initialize inverse property
  inv <- NULL
  set <- function(y) {
   x <<- y
  inv <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
  ## Way to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  ## Way to get the inverse of the matrix
  getInverse <- function() {
    ## Back the inverse property
    inv
  }
  ## Back a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
}
data <- x$getInverse()
inv <- solve(data, ...)
x$setinverse(inv)
inv
        ## Return a matrix that is the inverse of 'x'
}
