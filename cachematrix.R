## -----------------------------------------------------------------------------
## The following is a pair of functions that cache and compute the 
## inverse of a matrix.
## This function creates a special "myMatrix" object
## that can cache its inverse.
## -----------------------------------------------------------------------------

makeCacheMatrix = function(myMatrix = matrix()) {
  inverseMatrix = NULL   
  set = function(x) {    
    myMatrix <<- x;
    inverseMatrix <<- NULL;
  }
  get = function() return(myMatrix);    
  setinv = function(inv) inverseMatrix <<- inv; 
  getinv = function() return(inverseMatrix); 
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## -----------------------------------------------------------------------------
## This function computes the inverse of the special
## "myMatrix" returned by `makeCacheMatrix` above. If the inverseMatrix has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverseMatrix from the cache.
## -----------------------------------------------------------------------------
cacheSolve = function(myMatrix, ...) {
  inverseMatrix = myMatrix$getinv()
  if(!is.null(inverseMatrix)) {
    message("Getting cached data...")
    return(inverseMatrix)
  }
  data = myMatrix$get()
  invserse = solve(data, ...)
  myMatrix$setinv(inverseMatrix)
  return(inverseMatrix)
}