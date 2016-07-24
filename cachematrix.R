## Return the inverse of a matrix.
## get the cahed inverse if it exists, otherwise, calculate it. 
## Example to call: 
## load the functions into R: source("cachematrix.R")
## 1. Create a matrix: 
##    A <- matrix(runif(100,-1,1),nrow = 10, ncol=10) + diag(x=11,nrow=10,ncol=10) ## just to make sure that A is invertible

## 2. Create a special vector (a list) from A: 
##    A.list <- makeCacheMatrix(A)

## 3. Calculate its inverse: 
##    A.inverse <- cacheSolve(A.list)


## Write a short comment describing this function:
## this function creates a list of 4 elements, 
## which are actually 4 functions to set a matrix, to get a matrix, to get the inverse, to set the inverse
## of that matrix. 

makeCacheMatrix <- function(x = matrix()) 
{
  Inv <<- NULL
  
  set <- function(y) ## set the matrix value 
  {
    x <<- y
    Inv <<- NULL
  }
  get <- function()  ## get the input matrix
  {
    return(x)
  }
  setinv <- function(inverseMatrix) ## set the inverse 
  {
    Inv <<- inverseMatrix
  }
  getinv <- function()
  {
    return(Inv)
  }
  
  ## return a list of 4 elements: set, get, setinv, getinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
  



## Write a short comment describing this function
## this function calculates the inverse of a matrix, 
## but get the cached inverse matrix if it has already been calcualated. 
## Note: input is a list created by the function "makeCacheMatrix". 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inv <- x$getinv()
  if (is.null(Inv))
  {
    message("calculating the inverse matrix")
    Inv <- solve(x$get())    ## calculate the inverse matrix 
    x$setinv(Inv)     ## cache the inverse matrix. 
  }
  else
  {
    message("get cached inverse matrix")
  }
  return(Inv)
  
}
