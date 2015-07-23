## 2 functions: makeCacheMatrix to create matrix 'object' and cacheSolve to perfrom inversion and caching

## take a matrix as argument and store it together with access functions (get/set)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## on first call of given matrix 'object' (made by above function): calculate matrix inverse,
## store result and return result
## on later call (if result already stored): return stored result without new calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
