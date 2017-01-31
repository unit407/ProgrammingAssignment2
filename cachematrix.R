## Creates special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function()  x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(get = get, set = set,
       getinv = getinv,
       setinv = setinv)
  
}


## Creates inverse of the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <= x$getinv()
  if(!is.null(inv)) {
    messaged ("cached data coming")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
  }
