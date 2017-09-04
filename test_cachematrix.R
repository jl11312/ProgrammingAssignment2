## generateRandomInvertibleMatrix(size)
##    Generate a randcom non-singular matrix
##
##    Arguments
##       size: row/column size of the matrix to be generated
##
##    Returns
##       A random non-singular matrix of the specified size
##
generateRandomInvertibleMatrix <- function(size)
{
   # This function first generates an identity matrix of the specified size.  It then used the property that if M is a non-singular
   # matrix, then the matrix M' created by setting any row r1 to the value c1*r1 + c2*r2 (where c1 and c2 are arbitrary constants and r2
   # is a second row in the matrix) is also non-singular.
   #
   m <- diag(size)
   
   # Randomly determine the number of operations to be performed
   operationCount = sample(4*size, 1)
   
   operationIndex = 0
   while (operationIndex < operationCount)
   {
      # Randomly select r1 and r2
      #
      randomRow <- sample(size, 2, replace=FALSE)
      
      # Randomly select constants c1 and c2.  We keep these within a small range to avoid errors related to numeric precision
      # that could cause the matrix to become non-singular
      #
      randomConstant <- runif(2, 0.1, 2.0)
      
      # Apply the operation to the matrix
      m[randomRow[1], ] <- randomConstant[1]*m[randomRow[1],] + randomConstant[2]*m[randomRow[2], ]
      operationIndex <- operationIndex + 1
   }
   
   m
}

## testCacheMatrix()
##    Test the cached matrix data type and functions
##
##    Arguments
##       None
##
##    Returns
##       TRUE if all tests passed.  Otherwise, execution stops with an error message
##
## Warning - this test may require a significant amount of processor time on a slow computer. On a very fast computer, it may need
## to 
test_cacheMatrix <- function()
{
   # Create the cached matrix object to be used for the tests
   cachedM <- makeCacheMatrix()
   
   # The test uses processor time to determine if the inverse value was truly cached.  We need however to make sure that
   # the tested matrix is large enough so that the difference in computing the inverse and returning the cached value
   # can be reliably measured.  This value is set to TRUE if at least one of the tested sizes met this threshold.
   #
   atLeastOneTimeTest <- FALSE
   
   # Test a number of different matrix sizes
   #
   size <- 2
   while (size < 10000 & !atLeastOneTimeTest)
   {
      # Generate a random non-singular matrix and initialize the cached matrix object
      #
      m <- generateRandomInvertibleMatrix(size)
      cachedM$set(m)
      
      # At this point, the cached matrix object should have the correct matrix set and not have a cached inverse value
      #
      if (!identical(cachedM$get(), m)) stop("get() test failed for size ", size, call. = TRUE)
      if (!is.null(cachedM$getInverse())) stop("getInverse() pre-solve cache test failed for size ", size, call. = TRUE)
      
      # Solve for the inverse both by using solve directly and using the cacheSolve function
      mInverse <- solve(m)
      firstTime <- system.time(mTestInverse <- cacheSolve(cachedM))

      # At this point, the cached matrix object should still have the correct matrix set.  In addition, it should
      # have a cached inverse value
      #
      if (!identical(cachedM$get(), m)) stop("get() test failed for size ", size, call. = TRUE)
      if (is.null(cachedM$getInverse())) stop("getInverse() post-solve cache test failed for size ", size, call. = TRUE)
      
      # The inverse from calling solve directly and calling cacheSolve should be identical
      #
      if (!identical(mInverse, mTestInverse)) stop("getInverse() post-solve result test failed for size ", size, call. = TRUE)
      
      # Repeat the cacheSolve call
      #
      secondTime <- system.time(mTestInverse <- cacheSolve(cachedM))
      
      # Should still have a cached value for the inverse and the inverse value should remain identical to that from calling
      # solve directly
      #
      if (is.null(cachedM$getInverse())) stop("getInverse() post-solve cache test failed for size ", size, call. = TRUE)
      if (!identical(mInverse, mTestInverse)) stop("cacheSolve() test failed for size ", size, call. = TRUE)
      
      # Compute processor time for first and second calls to cache solve (user+system)
      firstTotal <- sum(firstTime[1:2])
      secondTotal <- sum(secondTime[1:2])

      # If the first call took at least 0.2 seconds, we should be able to detect a difference between the first and second call
      #
      if (firstTotal > 0.2)
      {
         # Assume that returning the cached value is at least twice as fast. In practice it will be a much larger difference,
         # but this is accurate enough for unit testing.
         #
         if (secondTotal > 0.5*firstTotal)
         {
            stop("cacheSolve does not appear to be caching ", size, call. = TRUE)
         }
         
         atLeastOneTimeTest <- TRUE
      }
      
      size <- size*2
   }

   # The test needs to fail if none of the specified sizes were large enough to test caching
   #
   if (!atLeastOneTimeTest) stop("test_cacheMatrix size not large enough for at least one timed test", call. = TRUE)
   
   TRUE
}
