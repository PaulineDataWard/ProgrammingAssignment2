## These functions will allow the caching of an inverse matrix, 
## and its retrieval.

## This function will create a special matrix object, and cache it!

makeCacheMatrix <- function(x = matrix()) {
  
  # If nothing has been cached as yet...
  if (cachedMatrix == NULL) {
    # Cache the matrix
    cachedMatrix <- x   
  }
    
  cachedMatrix
}


## This function will check whether a cached copy of an inverteds matrix exists:
## if so, immediately fetches it; if not, immediately inverts it!!!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve will retrieve the inverse from the cache.
  
  # If no matrix has been inverted and cached as yet...
  if (makeCachedMatrix(NULL) == NULL){
    # Invert the matrix and send it to be cached
    theAnswer <- solve(x)
    makeCacheMatrix(theAnswer)
    lastMatrixCached <- x
  }
  
  else {
    # Compare matrix received to the one that has been cached
    sameMatrixFlag <- NULL
    sameMatrixTest <- as.logical(lastMatrixCached == x)
    for (i in sameMatrixTest){
      if (!sameMatrixTest[i]){
        sameMatrixFlag <= FALSE
      }
    }
    
    if (!sameMatrixTest){
      # Invert the matrix and send it to be cached
      theAnswer <- solve(x)
      makeCacheMatrix(theAnswer)
    }
    
  }
    
  theAnswer
}
