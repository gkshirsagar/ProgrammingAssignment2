## cachematrix.R
##
## Implements a special matrix that can cache its inverse for speedy lookup.
##
## Usage:
## Call 'makeCacheMatrix()' to create a new matrix.
## Call 'cacheSolve()' to get the matrix's inverse.


## makeCacheMatrix()
## Creates the special matrix.
## Pass a regular square matrix in the call to create this matrix.
makeCacheMatrix <- function(m = matrix()) {
  # for caching the matrix's inverse
  i <- NULL

  # sets this matrix to a new matrix; invalidates the inverse
  set <- function(p) {
    m <<- p
    i <<- NULL
  }

  # return the underlying bare matrix
  get <- function() m

  # set the inverse of this matrix
  setSolve <- function(s) i <<- s

  # return the cached inverse of this matrix
  getSolve <- function() i

  # return a list disguised as the special matrix
  list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve()
## Computes the inverse of the special matrix created by makeCacheMatrix()
## and also stores the inverse back into the matrix for caching.
cacheSolve <- function(m, ...) {
  # get the cached inverse and see if it is valid
  i <- m$getSolve()

  if (!is.null(i)) {
    # it is valid so return it
    message("getting cached data")
    return(i)
  }

  # if the inverse is not valid, compute it using the actual matrix
  data <- m$get()
  i <- solve(data, ...)

  # cache it
  m$setSolve(i)

  # and return the inverse
  i
}
