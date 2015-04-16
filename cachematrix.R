## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix".
  
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setinverse<-function(inverse) inv <<- inverse
  getinverse<-function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # This function returns the inverse of a matrix passed as an argument.
  # This matrix must be created with the above function.
  # If the inverse has been previously computed, it is retrieved from
  # the cache, and the computation is skipped.
  
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  A<-x$get()
  inv<-solve(A)
  x$setinverse(inv)
  
  inv
}
