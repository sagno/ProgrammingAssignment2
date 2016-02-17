## The functions below calculate the inverse of a matrix enabling to cache it
## so to avoid repeating the calculation if it has been already performed before
## so to save computing time

## The makeCacheMatrix create a list of functions enabling to save the inverse (setinverse)
## and the matrix (set) in a seperate environment and rethrive them from that (getinverse and get).

makeCacheMatrix <- function(X = numeric()) {
  i <- NULL
  set <- function(Y) {
    X <<- Y
    i <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve calculate the inverse of the matrix X in case 
## it has not being calculate before, otherwise it take it from the cache
## and retrieve a message. If the matrix is changed with the above function 
## the inverse is also recalculate, since its value is set to null.

cacheSolve <- function(X, ...) {
  i <- X$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- X$get()
  i <- solve(data, ...)
  X$setinverse(i)
  i
}
