## These functions implement a caching mechanism for matrix inversion
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix,
## retrieving it from the cache if previously calculated or computing and storing it if not

## makeVector creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix
makeCacheMatrix <- function(xmatrix = matrix()) {
  inv <- NULL
  set <- function(new_matrix) {
    xmatrix <<- new_matrix
    inv <<- NULL
  }
  get <- function() xmatrix
  setinverse <- function(new_inv) inv <<- new_inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve: Calculates the inverse of the special "matrix" created with the
## function makeCacheMatrix.
## The function first checks to see if inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the 
## corresponding value of the special "matrix" in the cache via the setinverse 
## function.
cacheSolve <- function(xmatrix, ...) {
        ## Return a matrix that is the inverse of 'xmatrix'
  inv <- xmatrix$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- xmatrix$get()
  inv <- solve(data, ...)
  xmatrix$setinverse(inv)
  inv
}
