## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## Assign matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Fetches matrix
  get <- function() x
  ## Sets inverse to whatever is entered. DO NOT USE setinverse() directly
  setinverse <- function(solve) m <<- solve
  ## Fetches inverse of original matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## If inverse is already cached, this fetches the data from x$getinverse()
  if(!is.null(m)){
    message("Getting cached data.")
    return(m)
  }
  data <- x$get()
  ## Creates inverse matrix
  m <- solve(data, ...)
  ## Sets inverse to what was created by solve()
  x$setinverse(m)
  m
}
