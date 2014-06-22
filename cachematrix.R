## This script contains two functions able to cache potentially time 
## consumming computations. In this case, these functions are implemented
## to cache the inverse of a matrix and avoid recomputation tasks.

## Provides a set of functions to cache the matrix in memory, i.e.
## set and get all the information required (original matrix, cache inverse).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix. If the inverse has been
## previously computed, it is not computed but instead is readed 
## from memory using the functions within makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  print(inv)
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

