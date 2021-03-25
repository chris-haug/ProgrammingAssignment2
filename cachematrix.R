## This function creates an inverse of a matrix. Since this is a costly function
## it caches the result.

## makeCacheMatrix is a function that creates a "Matrix" obeject that can cache
## its inverse.
## -first it initalizzes an object "inv" and sets it to NULL
## -next set() is defined. it assigns its input "y" to "x" in the parent 
## environment and assigns the value of NULL to the inv object in the parent
## parent environment. This means that when the input to makeCacheMatrix is
## reset ("x") that the cached value is set back to NULL.
## -next, get() retreives "x" from the parent environment.
## -next, setinvr() assigns it's input argument "inverse" to "inv" so that it
## can be used in the parent environment
## -next, getinvr() retrives "inv" from the parent environment
## -finally, each part of the function is returned in a list so that it can be
## used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinvr <- function(inverse) inv <<- inverse
  getinvr <- function() inv
  list (set = set, get = get,
        setinvr = setinvr,
        getinvr = getinvr)
}


## cacheSolve uses makeCacheMatrix to compute the inverse of the matrix. If
## the inverse has already been solved and the matrix has not changed, then
## it will retrieve the cached inverse.

## -first, it attempts to retrieve the inverse of the object passed in in as "x"
## using getinvr() from the above function
## -next, if that result is not NULL (i.e. the the matrix "x" has been
## calculated previously), then it returns the inverse "inv" from the cache,
## notifiying the user in the process
## -next, if the result from getinvr() is NULL (i.e. the matrix "x" has not
## been calculated previoulsy), then cacheSolve() gets the matrix "x", 
## calculates the inverse using solve(), uses the setinvr() on "x" to set
## the inverse in "x"
## -finally, it prints the inverse "inv"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvr()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinvr(inv)
  inv
}
