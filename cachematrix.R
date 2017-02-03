##' Assignment: Caching the Inverse of a Matrix
##' Matrix inversion is usually a costly computation and there may be some 
##' benefit to caching the inverse of a matrix rather than compute it repeatedly 
##' (there are also alternatives to matrix inversion that we will not discuss here). 
##' 
##' Your assignment is to write a pair of functions that cache the inverse of a matrix.

##' Start:
##' 
##' This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(theMatrix = matrix()) {
  theInverse <- NULL
  
  setMatrix <- function(y) {
    theMatrix <<- y
    theInverse <<- NULL
  }
  getMatrix <- function() theMatrix
  setInverse <- function(newInverse) theInverse <<- newInverse
  getInverse <- function() theInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

##' This function computes the inverse of the special "matrix" returned 
##' by makeCacheMatrix above. If the inverse has already been calculated 
##' (and the matrix has not changed), then the cachesolve should retrieve the 
##' inverse from the cache.
cacheSolve <- function(aMatrix, ...) {
  ## Return a matrix that is the inverse of 'aMatrix'
  theInversedMatrix <- aMatrix$getInverse()
  if(!is.null(theInversedMatrix)) {
    message("Getting cached inverse")
    return(theInversedMatrix)
  }
  data <- aMatrix$getMatrix()
  theInversedMatrix <- solve(data)
  aMatrix$setInverse(theInversedMatrix)
  aMatrix$getInverse()
}

##' This function tests the functions in this R script
testMatrixCache <- function() {
  ## Create a 3x3 numeric test matrix and print it
  testMatrix <- matrix(rnorm(9, mean = 0, sd = 1),3,3)
  print("testMatrix=")
  print(testMatrix)
  
  ## Create a cached matrix and print it
  cachedMatrix <- makeCacheMatrix(testMatrix)
  print("cachedMatrix$getMatrix()=")
  print(cachedMatrix$getMatrix())
  
  ##Call the cacheSolve function to set and get the matrix' inverse, and print its structure
  theCachedMatrixInversed <- cacheSolve(cachedMatrix)
  print("theCachedMatrixInversed=")
  print(theCachedMatrixInversed)
  
  ##Test to see that the original and current matrices are identical
  x1 <- cachedMatrix$getMatrix()
  if (identical(testMatrix,x1)) {print("yes")} 
  else {
    print("no")
    print(testMatrix)
    print(x1)
  }
  
  ##Call the cacheSolve again to see that it returns the inverse from the cache
  print("Requesting the cached inverse again:")
  cacheSolve(cachedMatrix)
}

##testMatrixCache()