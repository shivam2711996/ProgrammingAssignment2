makeCacheMatrix <- function(x = matrix()) {
  ##Creates a square invertible matrix
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
    
    
    ##The function is really a list 
  ## 1. Sets value of  matrix 
  ## 2. Gets value of  matrix 
  ## 3. Sets  value of inverse 
  ## 4. Gets value of  inverse 
}
## This function computes the inverse of makeCacheMatrix
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse
}
