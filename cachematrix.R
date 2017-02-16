
## This function creates getters and setters for a matrix and a inverse matrix

makeCacheMatrix <- function(matrix = matrix()) {
  invMatrix <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    invMatrix <<- NULL
  }
  get <- function() matrix
  setInvMatrix <- function(newInvMatrix) invMatrix <<- newInvMatrix
  getInvMatrix <- function() invMatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## This function returns the inverse of a given matrix if it isn't 
## stored in cache yet and if it doesn't changed

cacheSolve <- function(matrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- matrix$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- matrix$get()
  invMatrix <- solve(data, ...)
  matrix$setInvMatrix(invMatrix)
  invMatrix
}
