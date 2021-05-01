## makeCacheMatrix() creates a matrix-like data structure that can be used to
## more efficiently obtain the value of the inverse of a matrix. cacheSolve()
## returns the inverse of a matrix. 

## creates and / or updates a matrix and caches the value of its inverse
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


## 'solve' a matrix by returning the cached value of its inverse if it exists
## or computing it using the solve() fn. if the inverse doesn't exist
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
