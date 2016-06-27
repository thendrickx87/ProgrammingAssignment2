## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Make cache matrix objects and it's functions to set the inverse of a matrix to the cache and get it from the cache.

makeCacheMatrix <- function(x = matrix()) {
#init the inverse to NULL
  cacheInverse <-NULL
  
  #Set matrix
  set <- function(y = matrix()){
    x<<-y #set x in cache
    cacheInverse <<-NULL # make cache empty
  }
  #Get matrix
  get<-function(){x}
  
  # get and set inverse 
  setInverse <- function(inv) {
    cacheInverse <<- inv #set the inverse
    return(cacheInverse) #return the inverse
  }
  
  #get function in order to get the cached inverse
  getInverse <- function() {cacheInverse}
  
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate the inverse of the matrix. 
# if the inverse is already calculated then it will be in the cache and we can read it out. If notting is in the cache, we have to calculate the inverse and it will be saved to the cache in order to prevent calculations afterwards. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check whether it is already calculated! 
  inv <- x$getInverse()
  # if the cache is not empty: write a message
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  #the inverse isnot yet calculated: calculate the inverse and save in cache

  matrix <- x$get() #get the matrix
	# calculate the inverse of the matrix
  inv <- solve(matrix)
  #save the inverse to the cache by using the setInverse()function
  x$setInverse(inv)
  #return the inverse.
  inv
}
