## several functions that cache the inverse of the matrix

## Creating a special matrix object to cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## getting the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Compute the inverse of the matrix 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of x
  m <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Getting the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse
  m <- solve(data) %*% data
  
  ## Setting the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}