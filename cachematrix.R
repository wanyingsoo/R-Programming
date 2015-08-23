## makeCacheMatrix function create the object that can cache the inverse of a matrix
## cacheSolve function tries to find the inverse of a matrix. If the result of the input matrix is already cached, cacheSolve will
## grab the result 

## makeCacheMatrix creates the object that can cache the input matrix x

makeCacheMatrix <- function(x = matrix()) { 
   i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve tries to find the inverse of the input matrix x. It first checks if the result is already cached 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
