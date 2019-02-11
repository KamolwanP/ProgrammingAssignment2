

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set<- function(y){
    x<<-y
    inverse<<-NULL
    
  }
  get<-function() x
  setinverse<-function(solve) inverse<<-solve
  getinverse<-function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
  if(!is.null(inverse))
  {
    message("getting cached data")
    return(inverse)
    
  }
  matrix<-x$get()
  inverse<-solve(matrix)
  x$setinverse(inverse)
  inverse
}