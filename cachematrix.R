## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  j <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  ## Method to get the matrix
  get <- function()
  {
    x
  }
  ## Method to set the inverse of matrix
  setInverse <- function(inverse)
  {
    j <<- inverse
  }
  ## Method to get the inverse matrix
  getInverse <- function() 
  {
    j
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  ## Just return the inverse if its already set
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  ## Get the matrix from our object
  k <- x$get()
  ## calculate the inverse
  j <- solve(k) %% k
  ## Set the inverse to the object
  x$setInverse(j)
  ## Return the matrix
  j
}