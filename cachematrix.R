## Matrix inversion is usually a costly computation.
## Below functions cache the inverse of a matrix rather than computing it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  matrix_inverse <- NULL ## initialize matrix
  
  ##These 4 functions and 2 vars exist in own environment.
  set <- function(y) { ## set the value of the matrix in own environment
    x <<- y
    matrix_inverse <<- NULL ##clear value
  }
  get <- function() x  ## return the value of the matrix provided as function argument
  set_inv <- function(inv) matrix_inverse <<- inv ## set the inverse of the matrix
  get_inv <- function() matrix_inverse ## get the inverse of the matrix
  
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv) #return the list of tag:function pairs
}


## This function(cacheSolve) computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##    cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'; checks first for inverse in cache AND that the matrix has not changed
  
  matrix_inverse <- x$get_inv()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
    }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$set_inv(matrix_inverse)
  matrix_inverse
}