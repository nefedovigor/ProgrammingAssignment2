## Below described two functions. First is ancillary one for caching of specified
## matrix and calcuted value of it by the next function.
## The second function does the calcuation of the inverted matrix (if needed) and puts
## it's value in cache

##First function

makeCacheMatrix <- function(x = matrix()) {
  ##Strarts the initialization of variable m.inverse and also resets it
  m.inverse <- NULL 
  
  ##defines the set() function for putting in cache original matrix. All variables
  ##are aviable in a parent environment
  set <- function(y) {
    x <<- y
    m.inverse <<- NULL ##Resets the value for calculation of the inversed matrix
  }
  
  get <- function() x ##Getter for the original matrix
  setInverse <- function(solve) m.inverse <<- solve ##Setter for calculated matrix
  getInverse <- function() m.inverse ##Getter for calculated matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Faunction for calculation and putting in cache

cacheSolve <- function(cache.matrix, ...) {
  ##Gets the cached data if it has been already calculated
  m.inverse <- cache.matrix$getInverse()
  if(!is.null(m.inverse)) {
    message("getting cached data")
    return(m.inverse) ##Returns the value 
  }
  
  ##There's no calculation has been done. So we're doing one
  data <- cache.matrix$get() ##Getting the original matrix
  m.inverse <- solve(data, ...)
  cache.matrix$setInverse(m.inverse) ##Putting the calculation in cache
  m.inverse ##Printing the result
}
