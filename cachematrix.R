## Put comments here that give an overall description of what your
## functions do

## This function take a matrix as an input and stores the inverse of the matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function (y){
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() {x}
  setInverseMatrix <- function(inverse) {inverseMatrix <<- inverse}
  getInverseMatrix <- function() {inverseMatrix}
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of a matrix and stores it in the cache if its not already stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if (!is.null(inverseMatrix)) return(inverseMatrix)
  matrixToInverse <- x$get()
  inverseMatrix <- solve(matrixToInverse, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
