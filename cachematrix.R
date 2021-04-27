## These functions can calculate a matrix inverse for the provided matrix.
## If the inverse has been calculated previoulsy, that inverse is cached for quick retrieval.

## Function to create a matrix object and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
          x <<- y
          m <<- NULL
      }
      get <- function()x
      set_matrix <- function(inverse) m <<- inverse
      get_matrix <- function() m
      list(set=set, get=get, set_matrix=set_matrix, get_matrix=get_matrix)
}


## Function to invert the matrix after checking to see that we can invert.
## Id the inverse has already been calculated, it will pull it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$get_matrix()
      if(!is.null(m)){
          message("getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$set_matrix(m)
      m
}
