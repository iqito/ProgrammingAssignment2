makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<- function(y) {
x<<-y
inv<<-NULL
}
get<-function()x  #function to get matrix x 
setinv<-function(inverse)inv<<-inverse
getinv<-function(){
  inver<-inv(x)
  inver%*%x
}
list(set = set, get = get,
     setinv= setinv,
     getinv = getinv)
}

cacheSolve <-function(x, ...)
{
  inv<-x$getinv()
  if(!is.null(inv)){
return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv  ##return a matrix that is the inverse of 'x'
}
