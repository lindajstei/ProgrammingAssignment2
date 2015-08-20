## Put comments here that give an overall description of what your
## functions do

## The functions store the inverse of a matrix in a cache, so that the inverse
## does not have to be repeatedly calculated.  The function cacheSolve calls
## the function makeCacheMatrix with the argument being the matrix to be 
## inverted.

## Write a short comment describing this function

## The function creates a special "matrix" which is a list of four functions.
## This list includes functions named set, get, setinverse, getinverse.
## 'set' sets the value of the matrix
## 'get' gets the value of the matrix
## 'setinverse' caches the value of the inverse of the matrix (created when it is
## called by cacheSolve)
## 'getinverse' finds the value of the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## This function calculates the inverse of a matrix and returns that inverse.
## If the inverse had been previously calculated it would have been stored in the 
## cache.  In that case, the function returns the cached inverse.  Otherwise,
## the function solves the inverse of the matrix and returns the calculated
## inverse, while setting the value of the inverse in the cache.
## This function calls the makeCacheMatrix function with the argument being
## the matrix that is to be inverted.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

