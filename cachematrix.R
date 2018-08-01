## This file has two functions:
## - makeCacheMatrix() to implement the 'cached matrix' 
## (rather a list of closure functions that share an environment)
## - cacheSolve() to invert the matrix or return the inverse from cache

## makeCacheMatrix accepts a matrix and returns a list with four elements:
## - get(): Function to get the value of the matrix mat
## - set(y): Function set the value of the matrix to y
## - getInverse(): Function to get the value of inv (it's supposed to be the inverse of mat)
## - setInverse(inverse): Function to set inv to inverse (it's supposed to be the inverse of mat)
## All of the above functions share the environment corresponding to their parent function
makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  inv <- NULL
  get <- function() mat
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  getInverse <- function() inv
  setInverse <- function(inverse) inv <<- inverse 
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## cacheSolve(x, ...) accepts a list created by makeCacheMatrix and returns the inverse of
## the contained matrix. If the operation has been performed before, it just returns the cached value.
## Otherwise, it calculates the inverse and stores it in the cache using setInverse()
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    print ("Returning value from cache...")
    return (inv)
  }
  else {
    inv <- solve(x$get(), ...)
    x$setInverse(inv)
    return (inv)
  }
}