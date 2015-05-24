## Function that cache the inverse of a matrix
## by Fillipe Massuda

## Function that creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## Set the matrix to the object
    set <- function(y){
      x <<- y
      m <<- NULL
    }
    
    ## get the matrix from the object
    get <- function() x
    
    ## set the inverse of the matrix
    setsolve <- function(solve) {
      m <<- solve
    } 
    
    ## get the inverse from the matrix
    getsolve <- function() m
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## If the inverse already exists, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from the object
  data <- x$get()
  
  ## Calculates the inverse using the solve function
  m <- solve(data, ...)
  
  ## add the inverse calculated to the cache
  x$setsolve(m)
  
  ## return the matrix
  m
}
