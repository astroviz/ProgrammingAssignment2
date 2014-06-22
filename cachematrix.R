## This script contains two functions the first creates a mechanism to cache the 
## solution for the inverse of a matrix.

## makeCacheMatrix calculates the inverse of matrix and stores it for
## access by other functions 

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function (y){
            x<<- y
            m<<-NULL
      }
      get <-function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function()
      list(set =set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)
}


## cacheSolve should check if the inverse of the matrix exists and if it does
## retrieve it from memory, if it does not then it should create the inverse 
## iteslf...unfortunately I couldn't get this to work !

cacheSolve <- function(x, ...) {
      m <- getinverse()
      if (!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m < - solve(data,...)
      x$setinverse(m)
      m
}
