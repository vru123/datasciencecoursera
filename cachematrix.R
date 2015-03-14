## Course : Coursera - R Programming
## Programming Assignment 2
## Author: Nitin Kaul
### Function Descriptions ###
## makeCacheMatrix - function creates a matrix and sets inverse calculation in cache
## cacheSolve - function computes inverse of matrix
### Example Usage ###
# source("cachematrix.R")   #Source the file
# mat1 <- makeCacheMatrix(matrix(1:4, 2))   #set a matrix   
# class(matrix1)    #check and confirm the class of mat1 as list
# mat1$get()    #get the created matrix details
# mat1$getInverse()   #checked the Iverse vaule is NULL
# mat1$set(matrix(5:8,2))   #set a new matrix using set function
# mat1$get()    #check if new matrix was set through set function
# cacheSolve(mat1)    #get the inverse of matrix and set the cache
# cacheSolve(mat1)    #check the inverse of matrix and inverse is being used from cache
# mat1$getInverse()   #check the inverset has been set in cache


## function makeCacheMatrix creates a special matrix object and can cache its inverse
## and returns a list of functions

################################################################################
# As suggeste below makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_x <<-inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
################################################################################ 
# As suggested below cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
#################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if (!is.null(inverse_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  } else {
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}
