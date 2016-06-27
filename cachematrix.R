## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cacheInverse <-NULL
  
  #Set matrix
  set <- function(y = matrix()){
    x<<-y #set x in cache
    cacheInverse <<-NULL
  }
  #Get matrix
  get<-function(){x}
  
  # get and set inverse 
  setInverse <- function(inv) {
    cacheInverse <<- inv
    return(cacheInverse)
  }
  getInverse <- function() {cacheInverse}
  
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check whether it is already calculated! 
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  #not yet calculated: calculate the inverse and save in cache
  matrix <- x$get() #get the matrix
  inv <- solve(matrix)
  x$setInverse(inv)
  inv
}
