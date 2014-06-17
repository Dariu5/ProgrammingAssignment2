## These two functions performs inverse matix calculation. 
## Results are cached and repeated computations are avoided.

## Creates function vector, that allow storage of variable in other enviroments

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
       setinverse= setinverse,
       getinverse = getinverse)

}


## Calculates inverse matrix, if it is already calculated gets result from cache 

cacheSolve <- function(x, ...) {
  
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