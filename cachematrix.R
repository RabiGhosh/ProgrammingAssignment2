## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## setinverse - stores the inverse in global variable mat
## getinverse - Returns the inverse from global variable mat

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- matrix(NULL, nrow=2, ncol=2)
  set <- function(y) {
    x <<- y
    mat <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) mat <<- inverse
  
  getinverse <- function() mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## for this excersize let's assume we are working with a 2 x 2 matrix
## call getinverse and if the inverse is in the cache it will return and that will be returned
## if getinverse does not return anything, the cache is empty
## store the input matrix in local variable data
## call solve() to inverse the matrix
## store the inverse the inverse in the global variable and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- matrix(NULL, nrow=2, ncol=2)
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
