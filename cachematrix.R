## A pair of functions that cache the inverse of a given square matrix
## If the matrix changes, the inverse will be re-calculated and cached.

##-----------------------------------------------------------------------------
## Create a special matrix that can cache its inverse
## (must be square to be invertible)
##-----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(inverse) matrixInverse <<- inverse
  getMatrixInverse <- function() matrixInverse
  list(set = set, 
       get = get, 
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)

}

##-----------------------------------------------------------------------------
## Compute the inverse of a square matrix that can cache its inverse
##-----------------------------------------------------------------------------
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getMatrixInverse()
        if(!is.null(matrixInverse)) {
                message("Grabbing cached data...")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data)
        x$setMatrixInverse(matrixInverse)
        matrixInverse
}
