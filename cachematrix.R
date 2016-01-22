## The below function will create special object to cache the inverse of a matrix in the 
## the global environment to avoid recomputing it if done already

## first function will create a special vector that contains all functions to create/get/store 
## the inverse of a matrixWrite a short comment 

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## the second function will return the inverse of the matrix by using the solve function
## but first check if computed already and stored in cache memory
## if this is the case, the function will return value from the cache
## otherwise will compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
