# makeCacheMatrix function returns list about cache matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
#get gives to original matrix
  
  get <- function() x  
  setinverse <- function(solve) m <<- solve
#getinverse gives to inverse of our original matrix
 
  getinverse <- function() m
#the below list is returned for makeCacheMatrix function

  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

#cacheSolve function computes the inverse returned by makeCacheMatrix.
#If the makeCacheMatrix did not give inverse or did not change the matrix, then cachesolve will give the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
