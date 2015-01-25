# This function creates a list that contains 4 member functions: set, get, setInv
# and getInv. It uses the <<- assignment operator so that these internal variables 
# are not exposed to the outside environment.

makeCacheMatrix <- function(x = matrix()) {
  
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
    
  }
  get <- function() x
  setInv <- function(Inv) xInv <<- Inv
  getInv <- function() xInv
  list(set = set, get = get, 
       setInv = setInv,
       getInv = getInv)
  }


# This function computes the inverse of the matrix returned by the
# makeCacheMatrix function above. If the inverse has already been calculated 
# and has not changed, then cacheSolve should retrieve the existing inverted 
# matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
