## These functions can be used to cache the inverse of an invertable matrix

## The makeCacheMatrix function Creates a special matrix object 
## that can cache its inverse. It stores a number of other 
## functions for setting and accessing the value of the cached
## inverse matrix.
##
## Args:
##    x: a matrix that can be inverted
##
##

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # variable to cache inverse of matrix.  
  
  ## The set funciton changes the value of the matrix x
  ## stored in the makeCacheMatrix function.
  ## It resets the value of the inverse i to null.
  set <- function(y) { 
    x <<- y      
    i <<- NULL   
  }
  
  ## The get function returns the matrix x stored in the main function 
  get <- function() x
  
  ## The setInverse function should be used to set the value of the
  ## inverse i stored in the main makeCacheMatrix function
  setInverse <- function(inverse) i <<- inverse 
  
  ## The getInverse function returns the value of the inverse
  ## i stored in the main makeCacheMatrix function
  getInverse <- function() i
  
  # the following line creates a list storing each of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This functin computes the inverse of the special matrix
## returned by the makeCacheMatrix function. If the inverse has
## already been calculated (and the matrix has not changed) then

## Args:
##    x: a makeCacheMatrix object
##
## Returns:
##    The inverse of the matrix stored in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()  # get inverse stored in makeCacheMatrix function
  
  if(!is.null(i)) {  # if inverse is already stored return it
    message("getting cached data")
    return(i)  
  }
  
  data <- x$get()   # get matrix stored in makeCacheMatrix function
  
  i <- solve(data, ...)  # calculate inverse of matrix
  
  x$setInverse(i) # set value of inverse stored in makeCacheMatrix function
  
  i # return value of inverse
        
}
