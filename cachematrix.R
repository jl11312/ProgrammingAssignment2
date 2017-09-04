## cachematrix.R provides a data type and supporting function to compute the inverse of a matrix and to cache the result
## to allow the inverse to be computed only once but used multiple times
##
## See also test_cachematrix.R which provides unit testing for this data type.

## makeCacheMatrix(x = matrix())
##   Create a data structure for caching a matrix and its inverse value
##
##   Arguments:
##      x: matrix to be cached
##
##   Returns:
##      A list of the get, set, getInverse, and setInverse functions
##
makeCacheMatrix <- function(x = matrix()) {
   # Initialize the cached inverse
   cachedInverse <- NULL
  
   # Set a new value for the cached matrix.  This also clears the cached value for the inverse,
   # forcing it to be re-computed the next time cacheSolve is called
   #
   set <- function(y)
   {
      x <<- y
      cachedInverse <<- NULL
   }
   
   # Get the value for the cached matrix
   get <- function() x
   
   # Set the cached value for the inverse of the cached matrix
   #
   setInverse <- function(inverse) cachedInverse <<- inverse

   # Get the cached value for the inverse of the cached matrix
   #
   getInverse <- function() cachedInverse

   # Return the list of functions for this data type 
   list(
      set=set,
      get=get,
      setInverse=setInverse,
      getInverse=getInverse
      )
}


## cacheSolve(x, ...)
##   Get the inverse of the specified cache matrix
##
##   Arguments
##      x: cached matrix for which the inverse is to be returned.  Must have been created with the
##         makeCacheMatrix function
##
##   Returns:
##      The inverse of the cached matrix
##
cacheSolve <- function(x, ...) {
   # Check if inverse has already been computed and cached
   #
   inverse <- x$getInverse()
   if (!is.null(inverse))
   {
      message("getting cached data")
      return(inverse)
   }
   
   # First call to cacheSolve for this matrix.  Need to compute and cache the inverse
   #
   data <- x$get()
   inverse <- solve(data, ...)
   x$setInverse(inverse)
   
   inverse
}
