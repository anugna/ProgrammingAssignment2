##the first function takes a matrix as input and returns a list of functions 
##to set/get the inverse of the input matrix whereas the second function checks 
##if the inverse is computed in which case it gets the cached inverse, otherwise
##it evaluates the inverse, sets it and returns it

## makeCacheMatrix takes a matrix as an input and returns an object of type list that has
##internal methods for getting cached inverse

makeCacheMatrix <- function(x = matrix()) {

 minverse<-NULL
 set <- function(y){
   x<<-y
   minverse<<-NULL
 }
  get <- function() x
 setinv<-function(inverse) { minverse<<-inverse}
   getinv<- function() minverse
 list(get=get,setinv=setinv,getinv=getinv) 
}


## cacheSolve takes an object created by the first function as input and checks if inverse is computed
## otherwise it solves for the inverse and sets it back in the original object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minverse<- x$getinv()
  if(!is.null(minverse))  {
    message("getting cached inverse")
    return(minverse)
  }
  mat<-x$get()
  minverse=solve(mat)
  x$setinv(minverse)
  minverse
}
