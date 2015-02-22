## Creates a function to generate a matrix and then a function to find its inverse. If the original matrix changes, then the matrix inverse is 
## recomputed. If the original matrix is unchanged, then the cached inverse is returned.
## Please excuse the excessive comments throughout - these are needed for me to keep track of what the heck is going on.

## This function creates a function that is stored as a variable

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # m tracks changes to the matrix, initialized to "NULL
      set <- function(y) # creates a variable "set" which is itself a function. This is used to populate the matrix
      {
            x <<- y
            m <<- NULL # sets m to NULL for update tracking
      }
      
      get <- function() x # returns the matrix 'x'
      setmatrix <- function(solve) m <<- solve 
      getmatrix <- function() m # returns 'm'. If x has changed then 'm' will be not NULL 
      list(get = get, set = set, getmatrix = getmatrix, setmatrix = setmatrix)
}
## This function tests for a change in the matrix and then returns either cached data or a recomputed inverse.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix() # gets the matrix from the 'makeCacheMatrix function
      if(!is.null(m)){ # tests to see if the matrix has changed. If not, then cached data is returned
            message("getting cached data")
            return(m)
      }
      matrix <- x$get() # recomputes inverse if the data has changed
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m # returns the matrix
}


