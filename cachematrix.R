## This piece of code tries to cache the inverse of a matrix. 
## The matrix is assumed to be square so that it can always be inversed.
## If an inverse of a matrix has been computed before.
## A matrix needs to be makde by makeCacheMatrix so that when
## cacheSolve is called, it can check whether the inverse has been 
## calculated.


## ---------------------------------------------------------------------
## The makeCacheMatrix function creates a list of functions
## ---------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  ## Set the matrix and assign NULL to the inverse
  setmatrix <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  ## Return the original matrix
  getmatrix <- function() x
  ## Calculate the inverse matrix
  setinverse <- function(solve) m_inv <<- solve
  ## Return the inverse matrix
  getinverse <- function() m_inv
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## ---------------------------------------------------------------------
## The cacheSolve function first check whether the matrix 'x'
## has been calculated, if so, it retrieves the cached inverse
## Otherwise, it retrieves the matrix 'x' and compute the inverse
## ---------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m_inv <- x$getinverse()
  
  ## If m_inv is not NULL, then it has been calculated.
  if (!is.null(m_inv)) {
    message ("Getting cached inverse")
    return (m_inv)
  }
  
  my_matrix <- x$getmatrix()
  message ("Calculating the inverse for the first time")
  m_inv <- solve(my_matrix, ...)
  x$setinverse(m_inv)
  m_inv
}
