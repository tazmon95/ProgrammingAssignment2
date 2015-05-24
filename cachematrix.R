## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  ## Extras :
    ##  Create a sample matrix:   mat <- replicate(y, rnorm(y)) ] where y is the x&y dimension of the matrix

    ## test the script
      ##  x <-makeCacheMatrix()
      ##  mat <- replicate(3, rnorm(3))
      ##  mat
      ##  x$set(mat)
      ##  x$get()
      ##  cacheSolve(x)
      ##  cacheSolve(x)
      ##

  ## makeCacheMatrix containes the functions to:
    ## 1. Set the values in a matrix
    ## 2. Get the values from the matrix
    ## 3. Set the values in the inverse matrix
    ## 4. Get the values from the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  ## initializes the inverse property of the function
  i <- NULL

  ## sub function to set the matrix
  set <- function(m) {
    x <<- m
    i <<- NULL
  }

  ## sub function to get the matrix
  get <- function() x

  ## sub function to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }

  ## sub function to get the inverse of the matrix
  getInverse <- function() i

  ## Return a list of the sub functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

  ## cacheSolve returns the invers of the matrix set in makeCacheMatrix  (the function assumes an invertible matrix)
    ## 1. First it checks to see if the inverse has already been calculated, if it has then it returns the stored value.
    ## 2. If the inverse has not already been calculated, then it computes the value and sets the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  y <- x$get()
  i <- solve(y)
  x$setInverse(i)
  i
}