##This function creates a special "matrix" object that can cache 
##its inverse. This object is in fact a list containing functions to:
##set the value of the matrix, get the value of the matrix,
##set the inverse of the matrix, get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y;
    inv <<- NULL  
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list( set = set, get = get , setinv = setinv, getinv = getinv )
  
}


## This function returns the inverse of the matrix by 
## checking to see if the inverse has already been computed.
## If so, it skips computation and gets the inverse of the matrix. 
## If not, it computes the matrix inverse and caches it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("now returning cached inverse matrix")
    return (inv)
  }
  else
  {
  
    message("computing matrix inverse and caching")
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    return (inv)
  }
}
