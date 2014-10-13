## Functions for caching results of matrix inversion to avoid
## potentially costly recacalculation.
## A small test case is included at the end.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(value) inverse <<- value
  getInverse <- function() inverse
  # return a list of functions, so we can call them later
  # on instances of this "matrix" object.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix,
## unless its inverse has already been calculated and cached.
## Always returns the matrix inverse of x.
## Caveat: assumes an invertible matrix is passed!
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Getting cached inverse...")
    return(inverse)
  }
  message("Computing inverse...")
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

## simple test case for checking cache generation and retrieval
## correctness
test <- function(x) {
  # 1. prepare the "matrix"
  message("Running test with: ", x)
  cm <- makeCacheMatrix(x)
  ourInverse <- cacheSolve(cm) # 2. compute the inverse
  cachedInverse <- cacheSolve(cm) # 3. repeat, forcing cache use
  rm(cm)
  # compare the results to each other and a direct call to solve()
  if (identical(ourInverse, cachedInverse) &
      identical(ourInverse, solve(x))) {
    print("Test succesfully completed!")
  } else {
    warning("Cached inverse does not match the calculated one!")
    print(ourInverse)
    print(cachedInverse)
    solve(x)
    stop()
  }
}
test(matrix(1:4, 2, 2))
test(matrix(c(1,3:9,11), 3, 3))
