## These two functions performs inverse matix calculation. 
## Results are cached and repeated computations are avoided.

## Creates function vector, that allow storage of variable in other enviroments

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse= setinverse,
       getinverse = getinverse)

}


## Calculates inverse matrix, if it is already calculated gets result from cache 

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
