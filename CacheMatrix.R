#creates a 'special' matrix
makeCacheSolve <- function(x=matrix()){
 i <- solve(x) 
 set <- function(y){
   x <<- y
   i <<- x
 }
  get<- function()x
  setsolve <- function(solve)  i<<- solve
  getsolve <- function()i
  list(set=set, get=get,
       setsolve= setsolve,
       getsolve= getsolve)
}


cacheSolve <- function(x, ...){
 
  inverse <- x$getsolve()
  if(is.matrix(inverse)) {
    message("getting cached data")
    return(inverse)
  }else{
  data <- x$get()
  i <- solve(data,...)
  x$setsolve(i)
  i
  }
}
  
#creates the inverse of the matrix in the cache. 
#it first checks to see if the INVERSE has already been calculated.
#If so, it gets the INVERSE from the cache and skips the computation.
#Otherwise, it calculates the INVERSE of the MATRIX and sets the value of the INVERSE in the cache.
