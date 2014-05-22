## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <-function(y){
    x<<- y
    I<<- NULL
  }
  get <- function()x
  setInverse <- function(Inverse) I <<-Inverse
  getInverse <- function() I
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function creates a list containing a function to:
## (1)Set the value of the matrix
## (2)Get the value of the matrix
## (3)Set the value of the inverse matrix
## (4)Get the value of the inverse matrix

cacheSolve <- function(x,...){
    I <- x$getInverse()
    if(!is.null(I)){
      message("getting cached data")
      return(I)
    }
    data <-x$get()
    I <- solve(data,...)
    x$setInverse(I)
    I
  }
  
##This function first attempts to get the inverse of the matrix. By
## using 'getInverse', if I is not NULL, it will return the message 
## getting cached data and return the value of I stored.
## However, if it is NULL, the inverse of the matrix will be 
## calculated, by first getting the value of the matrix, and using 
## 'solve' to solve for the inverse. It will then store the result
## into 'I' using the setInverse function. 