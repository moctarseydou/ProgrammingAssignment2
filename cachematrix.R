
# Assignment: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Below are a pair of functions that cache the inverse of a matrix.
# It is assumed that the matrix supplied is always invertible.

# This function creates a special "matrix" object that can cache its inverse
# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse property
  inv <- NULL
  # Setting the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getting the matrix
  get <- function() x
  
  # Setting the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Getting the inverse of the matrix
  getInverse <- function() inv
  
  # Returning a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Returning a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # Just returning the inverse if its already set
  if (!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  # Getting the matrix from our object
  mat <- x$get()
  # Calculate the inverse
  inv <- solve(mat, ...)
  # Setting the inverse to the object
  x$setInverse(inv)
  #Returning the inverse
  inv
}