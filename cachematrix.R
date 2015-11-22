## Functions below allow for caching the inverse of a matrix

## makeCacheMatrix function creates a list containing a function to
## 1) Set value of matrix
## 2) Get value of matrix
## 3) Set value of inverse of matrix
## 4) Get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
      setinverse=setinverse, 
      getinverse=getinverse)

}


## cacheSolve function returns the inverse of a matrix  
##If inverse has already been calculated (and the matrix has not been changed), then cacheSolve returns inverse from cache via setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
