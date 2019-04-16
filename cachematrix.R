## These functions allow the user to create a matrix, calculate the inverse of the 
## matrix, and store the inverse values into the system cache

## makeCacheMatrix works in the following steps:
## Creates a function with one argument x that is a matrix
## Creates an inverse Null variable
## Gets and sets the inverse of the matrix data
## Returns the values in the form of a list

makeCacheMatrix <- function(x = matrix()) {
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrix_inv <<- inv
  getInverse <- function() matrix_inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cachSolve calculates the inverse of the data created by the makeCacheMatrix function
## The function will examine if the matrix_inverse has already been calculated
## (aka. NOT NULL), then R will return the cached values.
## If R shows it has not been caclulated, then it will calculate and store the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getInverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setInverse(matrix_inverse)
  matrix_inverse
}