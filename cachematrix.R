## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
# set -#defines a function to set the matrix, x
# get returns the vector, x
# #setInverse set the , i, to im (inversed matrix)


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setInverse <- function(im) i <<-im
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.
# this function will compute the inverse matrix using solve and store it using setInverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  
  i<- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() # retrieve the original matrix
  i<- solve(data, ...)  #compute the inverse matrix using solve
  x$setInverse(i)
  i
}


