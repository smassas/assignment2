## Matrix functions (special and inverse)

## special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## initialize the inverse property
  i <- NULL
  
  ## method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## method the get matrix
  get <- function() {
    ## Return matrix
    m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    ## return inverse property
    i
  }
  
  ## return list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## return matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get matrix from object
  data <- x$get()
  
  ## calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## set the inverse to the object
  x$setInverse(m)
  
  ## return matrix
  m
}
