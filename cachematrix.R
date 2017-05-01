## This function creates a special vector, in the form of a list containing 
## a function to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse of the matrix, get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function finds the inverse of the special matrix created with the above function.
cacheSolve <- function(x, ...) {
  
  ## If the inverse has already been calculated and is stored in the cache
  ## the inverse is returned from the cache
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If not in cache, then the matrix is retrieved from user input, 
  ## its inverse is found and stored in the cache and returned as the value of the function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}