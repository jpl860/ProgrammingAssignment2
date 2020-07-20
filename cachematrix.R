## Put comments here that give an overall description of what your
## functions do

# For this assignment, the matrix supplied is always invertible.

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

rm(list=ls()) #removes all variables currently loaded into memory in R

# n: Square matrix order. Optional n:1:16 for a matrix 4x4
# mean=0
# sd: standard deviation

## @x: a square invertible matrix
## return: a list containing functions to
##   1. set the matrix ---set
##   2. get the matrix ---get
##   3. set the inverse --setinv
##   4. get the inverse --getinv
##   this list is used as the input to cacheSolve() function

# makeCacheMatrix <- function(x = matrix(),n=4,mean=0,sd=1)
makeCacheMatrix <- function(x = matrix()) {
  #x <-matrix(rnorm(n,mean,sd),nrow=n, ncol=n)
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment different from 
    # the current environment.
    x <<- y      
    inv <<- NULL
  }
  get <- function(){
    x
    }
  setinv = function(inverse){ 
    inv <<- inverse 
  }
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve` should retrieve the inverse from the cache.

## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
  
  # invx<-solve(x)
}



# To test the above functions, type in the console each one of the following commands
# pmat <-makeCacheMatrix(matrix(rnorm(10,0,2),nrow=4, ncol=4))
# pmat$get()
# pmat$getinv()
# --NULL
# cacheSolve(pmat)
# pmat$getinv()
