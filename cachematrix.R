makeCacheMatrix <- function(x = matrix()) {
  
  #Initalize the inverse property
  i <- NULL
  
  #method to set the matrix
  set <- function(y) {
    x <<- y*
    i <<- NULL
  }
  
  #method to get the matrix
  get <- function() x
  
  #method to set the inverse of matrx
  setinverse <- function(inverse) i <<- inverse
  
  #method to get the inverse of matrx
  getinverse <- function() i
  
  #returns list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  #if inverse already exists, return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #get matrix from the object
  data <- x$get()
  i <- solve(data,...)
  
  #set the inverse
  x$setinverse(i)
  
  #return the matrix
  i
}
