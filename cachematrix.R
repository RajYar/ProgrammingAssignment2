## These functions create matrix object that includes a cached matrix inverse
## When these fucntions are used to solve a matrix inverse, if the inverse is already cached,
## that is simply returned rather than solved.

## This augments standard matrix object with a cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

##  This function calculates the inverse of "matrix" created with the above 
##  function. However, it first checks to see if the inverse has already been calculated. If so, 
##  it gets the mean from the cache and skips the computation. Otherwise, it calculates the 
##  inverse of the matrix and sets the value of the inverse in cache using setinverse function.

cacheSolve <- function(x, ...) 
  {
  ## Return matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}