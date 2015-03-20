## Put comments here that give an overall description of what your
## functions do

## the function is a list of functions that creates a special vector, which is a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setInverse<-function(i) inverse<<-i
  getInverse<-function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function takes in a matrix and check if it's inverse has been computed,
## if true, it returns the stored inverse matrix,
## if faluse, it computes the inverse, stores the matrix and then returns it.
## the function is useful when we need to compute the inverse of a same matrix for multiple times
## especially if the matrix is complicated, it will save a lot of time.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
}
s=matrix(c(1,2,3,0,1,4,5,6,1),ncol=3)
d=makeCacheMatrix(s)
cacheSolve(d)
