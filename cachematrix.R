## Put comments here that give an overall description of what your
## functions do

## this is function that takes a matrix to be pass to another function and hold the inverted 
## matrix value.  The function create a list to hold and set value.


MakeCacheMatrix <- function(x=matrix())

  { 
  answer <- NULL
  set <- function(y) 
  {   
    x <<- y
    answer <<- NULL
  }
  
  get <- function() x
  setinvert <- function(invt) answer <<- invt
  getinvert <- function() answer
  list(set=set, get=get,
       setinvert=setinvert,
       getinvert=getinvert)
}

## This function will check if makeCacheMatrix has a already a cache inverted matrix
##(answer).  If it doesn't have a cached value, it will calculate the inversion value of 
## X and cache it in makeCacheMatrix for later retrival

cacheSolve <- function(x,...)
{
  answer <- x$getinvert()
  if(!is.null(answer)) {
    
    message("getting cached data")
    return(answer)
  }
  
  data <- x$get()
  answer <- solve(data,...)
  x$setinvert(answer)
  answer
}