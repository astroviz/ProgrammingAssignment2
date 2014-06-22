## This script contains two functions the first creates a mechanism to calculate
## the inverse of a matrix and cache the result.  the second function checks for
## a cached inverse and pulls the value it it exists, otherwise it calculates the
## inverse.  NOTE to help editors - I could not get either part to work so no need
## to test.

## makeCacheMatrix calculates the inverse of a square matrix using solve() and stores it for
## access by other functions 

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function (y){
            x<<- solve(y)
            m<<-NULL
      }
      get <-function() x
      setinverse <- function(cached) m <<- cached
      getinverse <- function()
      list(set =set, get=get, 
           setinverse=setinverse,
           getinverse=getinverse)
}


## cacheSolve should check if the inverse of the matrix exists and if it does
## retrieve it from memory, if it does not then it should create the inverse 
## iteslf

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
