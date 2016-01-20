## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## Example use: Suppose I want to calculate the inverse of the matrix
## given by matrix(c(4,2,7,6), nrow=2, ncol=2).
## I could first make a special cache matrix by typing the following
## into the terminal: mat<-makeCacheMatrix(matrix(c(4,2,7,6),nrow=2,ncol=2))

## To calculate or retrieve the inverse, I could then use cacheSolve
## to return the inverse by typing the following into the terminal:
## inv<-cacheSolve(mat)

###################################

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}
