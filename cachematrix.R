## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # This function creates a special "matrix", which is really a list of 
  # functions to set and get the values of the matrix, and its inverse.
  
  inv<-NULL # Initially the inverse has the NULL value.
  
  set<-function(y){ # With the 'set' function, the matrix values are 
    x<<-y           # assigned and cached. Also, the inverse is set
    inv<<-NULL      # to NULL, because if the matrix changes, its inverse
  }                 # may change too.
  
  get<-function() x # The 'get' function prints the matrix in the console.
  
  setinverse<-function(inverse) inv <<- inverse # The 'setinverse' function
                                                # assigns and caches the 
                                                # values of the inverse
                                                # matrix.
  
  getinverse<-function() inv # The 'getinverse' function prints the inverse
                             # matrix in the console.
  
  # The previous functions are returned as a list:
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'.
  
  # This function returns the inverse of a matrix passed as an argument,
  # which must be created with the above function and which is assumed to
  # be invertible.
  
  # If the inverse has been previously computed, it is retrieved from
  # the cache, and the computation is skipped.
  
  inv<-x$getinverse()                 # Firstly, the inverse of the
                                      # matrix passed as an argument is
  if(!is.null(inv)){                  # assigned to the 'inv' variable.
    message("getting cached data")    # If the value of the inverse is not
    return(inv)                       # NULL, it has been computed 
  }                                   # previously. In this case, the
                                      # previously obtained value 
                                      # is returned, as well as a message
                                      # indicating this fact.
  
  A<-x$get()        # Otherwise, the matrix is assigned to the 'A' variable,
  inv<-solve(A)     # and its inverse is computed using the 'solve' 
  x$setinverse(inv) # function. Its value is assigned to the 'inv' variable,
                    # and the inverse matrix is set, printing it in the 
  inv               # console.
  
}
