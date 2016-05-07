## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cInvMat <- NULL
  set <- function(y) {
    x <<- y 
    cInvMat <<- NULL
  }
  
  get <- function() x
  
  setInvMat <- function(invMat) {
    cInvMat <<- invMat 
    return(cInvMat)
  }
  
  getInvMat  <- function() cInvMat
  list(set=set, get=get, setInvMat=setInvMat, getInvMat=getInvMat)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()  
    existingInvMat <- x$getInvMat() 
  
  if(!is.null(existingInvMat) && prod(solve(existingInvMat)==data)) { 
    message("found cached inverse matrix:")
    return(existingInvMat)
  }
  InvMat <- solve(data)
  message("new inversed matrix:") 
  x$setInvMat(InvMat)
}
