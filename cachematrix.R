## cachematrix assignment no.2 
## Coursera - R Programming course r012
## Author: S. Giorgetti
## Date: 9 March 2015
## Version: 0.1
##
## This file contains funcions for defining a matrix object and computing and
## storing its inverse.
## The inverse is computed once and cached internally to be available
## to following calls

## makeCacheMatrix is the function for defining a matrix and storing it
## and its inverse. This function contains also the logics (getters and setters)
## for handling with the matrices
##
## input: x -- MUST be a matrix
## output: it is a list

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # cache for the inverse (initially set to NULL)
  
  #setter and getter for the original matrix
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  
  # setter and getter for the inverse matrix
  setinv <- function(minv = matrix()) xinv <<- minv
  getinv <- function() xinv
  
  # list for calling the methods above
  list(set = set, get = get,
       setInv = setinv,
       getInv = getinv)
}


## cacheSolve is the function for computing a matrix inverse and storing it in cache
## This function REQUIRES the peculiar above-defined list in input
##
## input: x -- MUST be of the type defined by makeCacheMatrix
## output: x -- it is the inverse of the matrix 'x$get()'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check whether the inverse of x was previously computed (and stored in x cache)
  # if yes, return the cached matrix
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  
  # compute the inverse of x and store it within x cache
  myMat <- x$get()
  matInv <- solve(myMat, ...)
  x$setInv(matInv)
  matInv # ah, OK! And finally return the inverse
}