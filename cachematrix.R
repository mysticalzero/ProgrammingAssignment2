## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to create a custom matrix object that could be used for
## caching computations.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    
    x <<- y
    inverse <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function
## Function to get the inverse of a given matrix.
## The function will only compute the inverse if
## it has not done so already. Otherwise, it will
## just return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Check if the inverse has already been computed
        inverse <- x[["getinverse"]]()
        if(!is.null(inverse)) {
          # Exist
          return(inverse)
          
        }
        
        # Does not exist
        inverse <- solve(x[["get"]]())
        x[["setinverse"]](inverse)
        inverse
}
